use std::collections::HashSet;

use gba_types::GBAIOError;

use crate::{rom::Rom, scripts::consts::SCRIPT_NAMES};

use super::consts::BYTES_TO_SKIP;

const MAX_SCRIPT_SIZE: usize = 200;

fn visit_script<T>(
    rom: &Rom,
    offset: usize,
    callback: impl Fn(u8, &[u8]) -> Option<T>,
) -> Result<Vec<T>, GBAIOError> {
    let mut results = vec![];
    let mut bytes_read = 0;

    while bytes_read < MAX_SCRIPT_SIZE {
        // Read the byte
        let byte = rom.read::<u8>(offset + bytes_read)?;
        bytes_read += 1;
        // Get the number of bytes to read after this one
        let skip = BYTES_TO_SKIP[byte as usize] as usize;
        // Read the bytes
        let bytes = &rom.data[offset + bytes_read..offset + bytes_read + skip];
        bytes_read += skip;

        // Call the callback
        if let Some(res) = callback(byte, bytes) {
            results.push(res);
        }

        // If the script is ending, stop
        match byte {
            // Commands that certainly end the script
            0x02 | 0x03 | 0x05 | 0x08 | 0x0C | 0x0D => break,
            _ => (),
        }
    }

    Ok(results)
}

pub fn print_script_commands(rom: &Rom, offset: usize) -> Result<(), GBAIOError> {
    // Print the script referenced by the offset
    let mut offsets: Vec<u32> = print_script(rom, offset)?;

    // Print the stuff it refernces if you haven't already printed it
    let mut printed: HashSet<u32> = HashSet::new();
    while let Some(offset) = offsets.pop() {
        if !printed.contains(&offset) {
            offsets.extend(print_script(rom, offset as usize)?);
            printed.insert(offset);
        }
    }

    Ok(())
}

/// Prints a single script and returns the scripts referenced by it
fn print_script(rom: &Rom, offset: usize) -> Result<Vec<u32>, GBAIOError> {
    use colored::Colorize;
    println!("{} {}", "@org".red(), format_offset(offset));

    visit_script(rom, offset, |code, bytes| {
        let mut args_str: String = "".into();
        let mut out_offset = None;

        if bytes.len() == 1 {
            args_str = format_value(bytes[0] as u32).to_string();
        } else if bytes.len() > 1 {
            args_str = match code {
                // call and goto
                0x04 | 0x05 => {
                    let offset = read_offset(bytes);
                    out_offset = Some(offset);
                    format_offset(offset as usize).to_string()
                }
                // call_if and goto_if
                0x06 | 0x07 => {
                    let offset = read_offset(&bytes[1..5]);
                    out_offset = Some(offset);
                    format!(
                        "{}, {}",
                        format_value(bytes[0] as u32),
                        format_offset(offset as usize)
                    )
                }
                _ => format!("{:02X?}", bytes),
            }
        }

        println!("    {} {}", SCRIPT_NAMES[code as usize].green(), args_str);
        out_offset
    })
}

#[inline(always)]
fn read_offset(bytes: &[u8]) -> u32 {
    u32::from_le_bytes(bytes.try_into().unwrap()) - 0x08000000
}

#[inline(always)]
fn format_value(value: u32) -> colored::ColoredString {
    use colored::Colorize;
    let str = format!("{}", value);
    str.yellow()
}

#[inline(always)]
fn format_offset(offset: usize) -> colored::ColoredString {
    use colored::Colorize;
    let str = format!("${:07X}", offset);
    str.cyan()
}
