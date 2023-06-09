use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    fmt::Display,
    hash::{Hash, Hasher},
};

use gba_types::GBAIOError;

use crate::rom::Rom;

use super::consts::BYTES_TO_SKIP;

const MAX_SCRIPT_SIZE: usize = 200;

#[derive(PartialEq, Eq, Hash, Clone, Ord)]
pub enum ScriptResource {
    Script(usize),
    MapScriptsTable(usize),
    Text(usize),
    Movement(usize),
    Products(usize),
}
impl ScriptResource {
    pub fn offset(&self) -> usize {
        use ScriptResource::*;
        match self {
            Script(offset) => *offset,
            Text(offset) => *offset,
            Movement(offset) => *offset,
            Products(offset) => *offset,
            MapScriptsTable(offset) => *offset,
        }
    }

    pub fn name(&self) -> String {
        use ScriptResource::*;
        match self {
            Script(_) => "Script",
            Text(_) => "Text",
            Movement(_) => "Movement",
            Products(_) => "Products",
            MapScriptsTable(_) => "Map Scripts Table",
        }
        .to_string()
    }

    /// Returns whether you can get more data from a resource
    /// (so, whether it is a branch or a leaf in the tree)
    pub fn is_expandable(&self) -> bool {
        use ScriptResource::*;
        match self {
            Script(_) => true,
            MapScriptsTable(_) => true,
            _ => false,
        }
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

        let name = match self {
            ScriptResource::Script(_) => "s",
            ScriptResource::Text(_) => "t",
            ScriptResource::Movement(_) => "m",
            ScriptResource::Products(_) => "p",
            ScriptResource::MapScriptsTable(_) => "mst",
        }
        .red();
        let offset = format!("${:X}", self.offset());

        let offset_with_color = if self.is_expandable() {
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
    pub fn read(rom: &Rom, offsets: Vec<ScriptResource>) -> Self {
        // Save the roots for later
        let roots = offsets.clone();

        // Keep a queue of offsets to visit
        let mut queue = offsets;
        // Keep track of the scripts you've already expanded
        let mut map: HashMap<ScriptResource, Vec<ScriptResource>> = HashMap::new();

        while let Some(parent_resource) = queue.pop() {
            if !parent_resource.is_expandable() {
                println!(
                    "[WARNING] Resource {:?} is not expandable. 
Support for non-expandable resources as roots is not implemented yet.",
                    parent_resource
                );
                continue;
            }

            // If you can already see this offset in the map, ignore it
            if map.contains_key(&parent_resource) {
                continue;
            }

            // Get the resources referenced by the script
            let resources = find_sub_references(rom, &parent_resource);

            // Add each script you encounter to the queue, and each resource to the map
            for resource in &resources {
                // If the resource can be expanded itself, add it to the queue
                if resource.is_expandable() {
                    if !map.contains_key(&resource) {
                        queue.push(resource.clone());
                    }
                } else {
                    map.insert(resource.clone(), vec![]);
                }
            }

            // Add the script to the expanded scripts
            map.insert(parent_resource, resources);
        }

        ScriptTree { map, roots }
    }
}

/// Visits a resource that can be explored and returns the resources it references
fn find_sub_references(rom: &Rom, resource: &ScriptResource) -> Vec<ScriptResource> {
    // This is find because the only times it will ever get here is
    // if the resource is expandable, which should only happen in
    // case of a script or a map scripts table.
    assert!(resource.is_expandable());

    if let ScriptResource::MapScriptsTable(offset) = resource {
        find_map_scripts_table_references(rom, *offset).unwrap_or(vec![])
    } else {
        find_script_references(rom, resource.offset()).unwrap_or(vec![])
    }
}

/// Does a visit in the script in which it collects the offsets of the
/// resources the script references.
fn find_script_references(
    rom: &Rom,
    offset: usize,
) -> Result<Vec<ScriptResource>, ScriptVisitError> {
    // Run a new visit in which you collect the offsets of the resources
    let v = visit_script(rom, offset, |code, bytes| {
        use ScriptResource::*;

        let result: Option<ScriptResource> = match code {
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

            // loadword, bufferstring, vbufferstring
            0x0F | 0x85 | 0xBF => ScriptResource::from_bytes(rom, &bytes[1..5], Text),

            // > Movement
            // applymovement
            0x4F | 0x50 => ScriptResource::from_bytes(rom, &bytes[2..6], Movement),

            // > Products
            // pokemart, pokemartdecoration, pokemartdecoration2
            0x86 | 0x87 | 0x88 => ScriptResource::from_bytes(rom, bytes, Products),

            // A special case for trainerbattle
            0x5C => {
                let battle_type = bytes[0];
                // The first 5 bytes are arguments unrelated to the resources

                let mut resources = vec![];
                // If the first argument is there, it is a text
                if bytes.len() >= 9 {
                    let res = ScriptResource::from_bytes(rom, &bytes[5..9], Text);
                    if res.is_some() {
                        resources.push(res.unwrap());
                    }
                }
                // If the second argument is there, it is a text
                if bytes.len() >= 13 {
                    let res = ScriptResource::from_bytes(rom, &bytes[9..13], Text);
                    if res.is_some() {
                        resources.push(res.unwrap());
                    }
                }
                // If the third argument is there, check what it should be
                if bytes.len() >= 17 {
                    let res = ScriptResource::from_bytes(
                        rom,
                        &bytes[13..17],
                        match battle_type {
                            TRAINER_BATTLE_CONTINUE_SCRIPT_NO_MUSIC
                            | TRAINER_BATTLE_CONTINUE_SCRIPT => Script,
                            _ => Text,
                        },
                    );
                    if res.is_some() {
                        resources.push(res.unwrap());
                    }
                }
                // If the fourth argument is there, it is always a script
                if bytes.len() >= 21 {
                    let res = ScriptResource::from_bytes(rom, &bytes[17..21], Script);
                    if res.is_some() {
                        resources.push(res.unwrap());
                    }
                }

                return Some(resources);
            }

            // Commands that do not reference any offset
            _ => return None,
        };

        if let Some(resource) = result {
            Some(vec![resource])
        } else {
            None
        }
    })?;

    // Compact the result
    let mut output: Vec<ScriptResource> = vec![];
    for vector in v {
        output.extend(vector);
    }

    Ok(output)
}

/// Does a visit of a map scripts table and collects the references to the
/// scripts it finds in there.
fn find_map_scripts_table_references(
    rom: &Rom,
    offset: usize,
) -> Result<Vec<ScriptResource>, GBAIOError> {
    // Start reading the table
    let mut res = vec![];
    let mut read_size = 0;
    loop {
        // Read the first variable
        if rom.read_unaligned_halfword(offset + read_size)? == 0 {
            break;
        }
        // Skip the first two variables
        read_size += 4;

        // Read the script
        let script = rom.read_unaligned_offset(offset + read_size)?;
        res.push(ScriptResource::Script(script as usize));
        read_size += 4;
    }

    Ok(res)
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
            if key.is_expandable() {
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

const TRAINER_BATTLE_SINGLE: u8 = 0;
const TRAINER_BATTLE_CONTINUE_SCRIPT_NO_MUSIC: u8 = 1;
const TRAINER_BATTLE_CONTINUE_SCRIPT: u8 = 2;
const TRAINER_BATTLE_SINGLE_NO_INTRO_TEXT: u8 = 3;
const TRAINER_BATTLE_DOUBLE: u8 = 4;
const TRAINER_BATTLE_REMATCH: u8 = 5;
const TRAINER_BATTLE_CONTINUE_SCRIPT_DOUBLE: u8 = 6;
const TRAINER_BATTLE_REMATCH_DOUBLE: u8 = 7;
const TRAINER_BATTLE_CONTINUE_SCRIPT_DOUBLE_NO_MUSIC: u8 = 8;
const TRAINER_BATTLE_PYRAMID: u8 = 9;
const TRAINER_BATTLE_SET_TRAINER_A: u8 = 10;
const TRAINER_BATTLE_SET_TRAINER_B: u8 = 11;
const TRAINER_BATTLE_HILL: u8 = 12;

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
            // Read the type byte
            let battle_type = *rom
                .data
                .get(offset + bytes_read)
                .ok_or(ScriptVisitError::ReadOutOfBounds(offset))?;

            5 + match battle_type {
                TRAINER_BATTLE_SINGLE => 8,
                TRAINER_BATTLE_CONTINUE_SCRIPT_NO_MUSIC => 12,
                TRAINER_BATTLE_CONTINUE_SCRIPT => 12,
                TRAINER_BATTLE_SINGLE_NO_INTRO_TEXT => 4,
                TRAINER_BATTLE_DOUBLE => 12,
                TRAINER_BATTLE_REMATCH => 8,
                TRAINER_BATTLE_CONTINUE_SCRIPT_DOUBLE => 16,
                TRAINER_BATTLE_REMATCH_DOUBLE => 12,
                TRAINER_BATTLE_CONTINUE_SCRIPT_DOUBLE_NO_MUSIC => 16,
                TRAINER_BATTLE_PYRAMID => 8,
                TRAINER_BATTLE_SET_TRAINER_A => 8,
                TRAINER_BATTLE_SET_TRAINER_B => 8,
                TRAINER_BATTLE_HILL => 8,
                _ => 0,
            }
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
    pub fn from_bytes<F>(rom: &Rom, bytes: &[u8], f: F) -> Option<Self>
    where
        F: FnOnce(usize) -> Self,
    {
        let offset = u32::from_le_bytes(bytes[0..4].try_into().unwrap()) as usize;
        if offset < 0x08000000 || offset > 0x08000000 + rom.size() {
            None
        } else {
            Some(f(offset - 0x08000000))
        }
    }
}
