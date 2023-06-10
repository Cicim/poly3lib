use std::process::exit;

use gba_macro::gba_struct;
use poly3lib::rom::Rom;

gba_struct!(Struct {
    u8 a;
    u16 b;
    u32 c;
});

fn main() {
    // Open the ROM
    let rom = open_rom();
    println!();

    // Read the offset from the second argument
    let args: Vec<String> = std::env::args().collect();
    let offset = &args.get(2);

    if offset.is_none() {
        println!("Usage: poly3 <rom> <offset>");
        exit(1);
    }
    let offset = offset.unwrap();
    let offset = usize::from_str_radix(offset, 16).unwrap();

    macro_rules! read_and_print {
        ($offset:expr, $ty:ty) => {
            match rom.read::<$ty>($offset) {
                Ok(value) => println!("{} at offset {:#08X}:\n  {:?}", 
                    stringify!($ty), $offset, value),
                Err(err) => println!("Failed to read {} at offset {:#08X}: {}", 
                    stringify!($ty), $offset, err),
            }
        };
    }

    read_and_print!(offset, u8);
    read_and_print!(offset, u16);
    read_and_print!(offset, u32);
    read_and_print!(offset, i8);
    read_and_print!(offset, i16);
    read_and_print!(offset, i32);

    read_and_print!(offset, [u8; 4]);
    read_and_print!(offset, [u16; 4]);
    read_and_print!(offset, [u32; 4]);
    read_and_print!(offset, [i8; 4]);
    read_and_print!(offset, [i16; 4]);
    read_and_print!(offset, [i32; 4]);

    read_and_print!(offset, Struct);

    read_and_print!(offset, gba_types::colors::GBAPalette);
}

fn open_rom() -> Rom {
    // Get the ROM name from arguments
    let args: Vec<String> = std::env::args().collect();
    let rom_name = &args.get(1);

    if rom_name.is_none() {
        println!("Usage: poly3 <rom> <offset>");
        exit(1);
    }
    let rom_name = rom_name.unwrap();

    // Load the ROM
    let rom = Rom::load(rom_name);

    match rom {
        Ok(rom) => {
            println!("ROM loaded successfully!");
            println!("ROM type: {:?}", rom.rom_type);
            println!(
                "ROM size: {} bytes ({} MB)",
                rom.size(),
                rom.size() / 1024 / 1024
            );

            return rom;
        }
        Err(err) => match err {
            poly3lib::rom::RomError::InvalidRom => {
                println!("Invalid ROM file");
                exit(2);
            }
            poly3lib::rom::RomError::IoError(err) => {
                println!("IO error: {}", err);
                exit(3);
            }
        },
    };
}
