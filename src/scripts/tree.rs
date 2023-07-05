use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    fmt::Display,
    hash::{Hash, Hasher},
};

use crate::rom::Rom;

use super::consts::BYTES_TO_SKIP;

const MAX_SCRIPT_SIZE: usize = 200;

#[derive(PartialEq, Eq, Hash, Clone, Ord)]
pub enum ScriptResource {
    Script(u32),
    Text(u32),
    Movement(u32),
    TrainerBattle(u32),
    Products(u32),

    InvalidPointer(u32),
}
impl ScriptResource {
    pub fn offset(&self) -> u32 {
        use ScriptResource::*;
        match self {
            Script(offset) => *offset,
            Text(offset) => *offset,
            Movement(offset) => *offset,
            TrainerBattle(offset) => *offset,
            Products(offset) => *offset,

            InvalidPointer(offset) => *offset,
        }
    }

    pub fn name(&self) -> String {
        use ScriptResource::*;
        match self {
            Script(_) => "Script",
            Text(_) => "Text",
            Movement(_) => "Movement",
            TrainerBattle(_) => "TrainerBattle",
            Products(_) => "Products",

            InvalidPointer(_) => "InvalidPointer",
        }
        .to_string()
    }
}

impl std::fmt::Debug for ScriptResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(${:08X})", self.name(), self.offset())
    }
}

impl Display for ScriptResource {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use colored::Colorize;

        let name = self.name()[0..1].to_lowercase().red();
        let offset = format!("${:X}", self.offset());

        let offset_with_color = if let ScriptResource::Script(_) = self {
            // Find the color of the hash
            let mut hasher = DefaultHasher::new();
            self.hash(&mut hasher);
            let hash = hasher.finish();

            // Get a random color using the hash
            let r: u8 = ((hash >> 16) & 0xFF) as u8;
            let g: u8 = ((hash >> 8) & 0xFF) as u8;
            let b: u8 = (hash & 0xFF) as u8;

            let bg = offset.on_truecolor(r, g, b);
            let luma = r as f32 * 0.299 + g as f32 * 0.587 + b as f32 * 0.114;
            if luma > 179.0 {
                bg.truecolor(0, 0, 0).to_string()
            } else {
                bg.truecolor(255, 255, 255).to_string()
            }
        } else {
            offset
        };

        write!(f, "{}:{}", name, offset_with_color)
    }
}

impl PartialOrd for ScriptResource {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.offset().cmp(&other.offset()))
    }
}

#[derive(Debug)]
pub struct ScriptTree {
    pub(crate) map: HashMap<ScriptResource, Vec<ScriptResource>>,
    pub(crate) roots: Vec<ScriptResource>,
}

impl ScriptTree {
    pub fn read(rom: &Rom, offsets: Vec<usize>) -> Self {
        // Save the roots for later
        let roots = offsets
            .clone()
            .into_iter()
            .map(|offset| ScriptResource::Script(offset as u32))
            .collect();

        // Keep a queue of offsets to visit
        let mut queue = offsets;
        // Keep track of the scripts you've already expanded
        let mut map: HashMap<ScriptResource, Vec<ScriptResource>> = HashMap::new();

        while let Some(script_offset) = queue.pop() {
            // If you can already see this offset in the map, ignore it
            if map.contains_key(&ScriptResource::Script(script_offset as u32)) {
                continue;
            }

            // Get the resources referenced by the script
            let resources = find_script_references(rom, script_offset).unwrap_or(vec![]);

            // Add each script you encounter to the queue, and each resource to the map
            for resource in &resources {
                if let ScriptResource::Script(offset) = resource {
                    if !map.contains_key(resource) {
                        queue.push(*offset as usize);
                    }
                } else {
                    map.insert(resource.clone(), vec![]);
                }
            }

            // Add the script to the expanded scripts
            map.insert(ScriptResource::Script(script_offset as u32), resources);
        }

        ScriptTree { map, roots }
    }
}

/// Does a visit in the script in which it collects the offsets of the
/// resources the script references.
fn find_script_references(
    rom: &Rom,
    offset: usize,
) -> Result<Vec<ScriptResource>, ScriptVisitError> {
    // Run a new visit in which you collect the offsets of the resources
    visit_script(rom, offset, |code, bytes| {
        use ScriptResource::*;

        Some(match code {
            // > Other Scripts
            // call and goto
            0x04 | 0x05 => ScriptResource::from_bytes(rom, bytes, Script),
            // call_if and goto_if
            0x06 | 0x07 => ScriptResource::from_bytes(rom, &bytes[1..5], Script),

            // > Texts
            // message, braillemessage, messageautoscroll, vmessage, vbuffermessage,
            // loadhelp, messageinstant, pokenavcall
            0x67 | 0x78 | 0x9B | 0xBD | 0xBE | 0xC8 | 0xDB | 0xDF => {
                ScriptResource::from_bytes(rom, bytes, Text)
            }
            // bufferstring, vbufferstring
            0x85 | 0xBF => ScriptResource::from_bytes(rom, &bytes[1..5], Text),

            // > Movement
            // applymovement
            0x4F | 0x50 => ScriptResource::from_bytes(rom, &bytes[2..6], Movement),

            // > Products
            // pokemart, pokemartdecoration, pokemartdecoration2
            0x86 | 0x87 | 0x88 => ScriptResource::from_bytes(rom, bytes, Products),

            // Commands that do not reference any offset
            _ => return None,
        })
    })
}

impl Display for ScriptTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Sort the keys by offset
        let mut keys: Vec<&ScriptResource> = self.map.keys().collect();
        keys.sort();

        // Loop through all the resources
        writeln!(f, "Script Tree:")?;
        write!(f, " Roots:")?;
        for key in &self.roots {
            write!(f, " {}", key)?;
        }
        writeln!(f)?;

        writeln!(f, " Dependency relations:")?;
        for key in keys {
            if let ScriptResource::Script(_) = key {
                // For scripts, also print the referenced resources
                let vec = self.map.get(key).unwrap();

                // If this is a root
                if self.roots.contains(key) {
                    write!(f, "  r{} -> [ ", key)?;
                } else {
                    write!(f, "  {} -> [ ", key)?;
                }
                for res in vec {
                    write!(f, "{} ", res)?;
                }
                writeln!(f, "]")?;
            } else {
                writeln!(f, "  {}", key)?;
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum ScriptVisitError {
    ReadOutOfBounds(usize),
    UnknownCommand(u8),
}

/// Generic function for visiting a script.
///
/// Takes a callback that is called for each command in the script,
/// and receives the command code and the bytes that follow it.
///
/// It builds a vector of the results of the callback (ignoring `None`s).
pub fn visit_script<T>(
    rom: &Rom,
    offset: usize,
    f: impl Fn(u8, &[u8]) -> Option<T>,
) -> Result<Vec<T>, ScriptVisitError> {
    let mut results = vec![];
    let mut bytes_read = 0;

    while bytes_read < MAX_SCRIPT_SIZE {
        // Read the byte
        let byte = *rom
            .data
            .get(offset + bytes_read)
            .ok_or(ScriptVisitError::ReadOutOfBounds(offset))?;

        bytes_read += 1;

        // Get the number of bytes to read after this one
        let skip = if byte == 0x5C {
            todo!("Implement skip of trainerbattle")
        } else if byte >= 0xE2 {
            return Err(ScriptVisitError::UnknownCommand(byte));
        } else {
            BYTES_TO_SKIP[byte as usize] as usize
        };

        // Read the bytes
        let bytes = rom
            .data
            .get(offset + bytes_read..offset + bytes_read + skip)
            .ok_or(ScriptVisitError::ReadOutOfBounds(offset))?;
        bytes_read += skip;

        // Call the callback
        if let Some(res) = f(byte, bytes) {
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

impl ScriptResource {
    /// Convert bytes into the offset for a specific resource.
    ///
    /// Assumes the input bytes are 4. Panics otherwise.
    ///
    /// # Example
    /// ```
    /// use poly3lib::scripts::visit::ScriptResource;
    /// use poly3lib::rom::Rom;
    ///
    /// let rom = Rom::load("roms/firered.gba").unwrap();
    ///
    /// assert_eq!(ScriptResource::from_bytes(&rom, &[0, 0, 0, 8], ScriptResource::Script),
    ///     ScriptResource::Script(0));
    ///
    /// assert_eq!(ScriptResource::from_bytes(&rom, &[0, 0, 0, 0xFF], ScriptResource::Text),
    ///     ScriptResource::InvalidPointer(0xFF00_0000));
    /// ```
    pub fn from_bytes<F>(rom: &Rom, bytes: &[u8], f: F) -> Self
    where
        F: FnOnce(u32) -> Self,
    {
        let offset = u32::from_le_bytes(bytes[0..4].try_into().unwrap());
        if offset < 0x08000000 || offset > 0x08000000 + rom.size() as u32 {
            Self::InvalidPointer(offset)
        } else {
            f(offset - 0x08000000)
        }
    }
}
