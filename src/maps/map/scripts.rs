use serde::{Deserialize, Serialize};

use rom_data::{types::RomReadableType, Offset, RomData, RomIoError};

const ON_LOAD: u8 = 1;
const ON_FRAME: u8 = 2;
const ON_TRANSITION: u8 = 3;
const ON_WARP_INTO_MAP: u8 = 4;
const ON_RESUME: u8 = 5;
const ON_DIVE_WARP: u8 = 6;
const ON_RETURN_TO_FIELD: u8 = 7;
const MAX_TYPE_SIZE: usize = (ON_RETURN_TO_FIELD as usize) * 5 + 1;

// ANCHOR Map Scripts
/// The map scripts read from a map header.
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct MapScripts {
    offset: Offset,
    read_size: usize,

    /// Script with id `ON_LOAD` (`0x1`)
    load: Option<Offset>,
    /// Scripts table with id `ON_FRAME` (`0x2`)
    frame: Option<MapScriptsSubTable>,
    /// Script with id `ON_TRANSITION` (`0x3`)
    transition: Option<Offset>,
    /// Scripts table with id `ON_WARP_INTO_MAP` (`0x4`)
    warp_into_map: Option<MapScriptsSubTable>,
    /// Script with id `ON_RESUME` (`0x5`)
    resume: Option<Offset>,
    /// Script with id `ON_DIVE_WARP` (`0x6`)
    dive_warp: Option<Offset>,
    /// Script with id `ON_RETURN_TO_FIELD` (`0x7`)
    return_to_field: Option<Offset>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct MapScriptsSubTable {
    offset: Offset,
    read_size: usize,
    scripts: Vec<ScriptWithVars>,
}

#[derive(Debug, Serialize, Deserialize, Clone, Copy)]
/// The variables to be compared to execute a script.
pub struct ScriptWithVars {
    first_var: u16,
    second_var: u16,
    script: Offset,
}

// ANCHOR Reading
impl RomReadableType for MapScripts {
    fn read_from(rom: &RomData, offset: Offset) -> Result<Self, RomIoError> {
        // All script types
        let mut load = None;
        let mut frame = None;
        let mut transition = None;
        let mut warp_into_map = None;
        let mut resume = None;
        let mut dive_warp = None;
        let mut return_to_field = None;

        // The form is that of a variable-length list of entries consisting
        // if one byte for type and then a pointer to the script.
        // Only the first script of each type is read.
        let mut read_size = 0;

        while read_size < MAX_TYPE_SIZE {
            let kind = rom.read_byte(offset + read_size)?;
            read_size += 1;
            // If the kind is 0 (table end) or unknown, stop reading here.
            if kind == 0 || kind > ON_RETURN_TO_FIELD {
                break;
            }

            let ptr = match rom.read_offset(offset + read_size) {
                Ok(ptr) => ptr,
                Err(RomIoError::ReadingInvalidPointer(_, _)) => continue,
                err => err?,
            };
            read_size += 4;

            match kind {
                // Tables
                ON_FRAME => frame = Some(MapScriptsSubTable::read_from(rom, ptr)?),
                ON_WARP_INTO_MAP => warp_into_map = Some(MapScriptsSubTable::read_from(rom, ptr)?),

                // Simple scripts
                ON_LOAD => load = Some(ptr),
                ON_TRANSITION => transition = Some(ptr),
                ON_RESUME => resume = Some(ptr),
                ON_DIVE_WARP => dive_warp = Some(ptr),
                ON_RETURN_TO_FIELD => return_to_field = Some(ptr),

                _ => unreachable!(),
            }
        }

        Ok(Self {
            offset,
            read_size,

            load,
            frame,
            transition,
            warp_into_map,
            resume,
            dive_warp,
            return_to_field,
        })
    }
}

impl RomReadableType for MapScriptsSubTable {
    fn read_from(rom: &RomData, offset: Offset) -> Result<Self, RomIoError> {
        let mut scripts = Vec::new();
        let mut read_size = 0;

        while read_size < MAX_TYPE_SIZE {
            let first_var = rom.read_halfword(offset + read_size)?;
            read_size += 2;
            // If the first var is 0, stop reading here.
            if first_var == 0 {
                break;
            }

            let second_var = rom.read_halfword(offset + read_size)?;
            read_size += 2;
            let script = match rom.read_offset(offset + read_size) {
                Ok(ptr) => ptr,
                Err(RomIoError::ReadingInvalidPointer(_, _)) => continue,
                err => err?,
            };
            read_size += 4;

            scripts.push(ScriptWithVars {
                first_var,
                second_var,
                script,
            });
        }

        Ok(Self {
            offset,
            read_size,
            scripts,
        })
    }
}

// ANCHOR Clearing
impl MapScripts {
    pub fn clear(&self, rom: &mut RomData) -> Result<(), RomIoError> {
        rom.clear_bytes(self.offset, self.read_size)?;

        if let Some(frame) = &self.frame {
            rom.clear_bytes(frame.offset, frame.read_size)?;
        }
        if let Some(warp_into_map) = &self.warp_into_map {
            rom.clear_bytes(warp_into_map.offset, warp_into_map.read_size)?;
        }

        Ok(())
    }
}

// ANCHOR Getting offsets
impl MapScripts {
    /// Get the scripts' offsets referenced by this struct.
    pub fn get_scripts(&self) -> Vec<Offset> {
        let mut scripts = Vec::new();

        // Direct scripts
        if let Some(load) = self.load {
            scripts.push(load);
        }
        if let Some(transition) = self.transition {
            scripts.push(transition);
        }
        if let Some(resume) = self.resume {
            scripts.push(resume);
        }
        if let Some(dive_warp) = self.dive_warp {
            scripts.push(dive_warp);
        }
        if let Some(return_to_field) = self.return_to_field {
            scripts.push(return_to_field);
        }

        // Tables
        if let Some(frame) = &self.frame {
            for script in &frame.scripts {
                scripts.push(script.script);
            }
        }
        if let Some(warp_into_map) = &self.warp_into_map {
            for script in &warp_into_map.scripts {
                scripts.push(script.script);
            }
        }

        scripts
    }
}
