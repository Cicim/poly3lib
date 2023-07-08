use std::fmt::Display;

use serde::Serialize;

use crate::{
    refs::{TableInitError, TablePointer},
    rom::{Rom, RomType},
};

#[derive(Debug, Serialize)]
pub struct MapSectionDump {
    start_index: u8,
    none_index: u8,
    names: Vec<Option<String>>,
}

impl Display for MapSectionDump {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use colored::Colorize;

        writeln!(f, "Map Section Names")?;
        for (i, name) in self.names.iter().enumerate() {
            let mid = i + self.start_index as usize;
            let mid = format!("    ${:02X}", mid).green();

            if let Some(name) = name {
                writeln!(f, "{}: {}", mid, name)?
            } else {
                let none = "None".red();
                writeln!(f, "{}: {}", mid, none)?
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum MapSectionError {
    NotInitialized,
    NoMapsecBounds,
    NoShiftAndScale,
}

impl Display for MapSectionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use MapSectionError::*;

        match self {
            NotInitialized => write!(f, "Map section table not initialized"),
            NoMapsecBounds => write!(f, "No map section bounds found"),
            NoShiftAndScale => write!(f, "No shift and scale found"),
        }
    }
}

pub struct MapSectionTable<'rom> {
    pub rom: &'rom Rom,
}

impl<'rom> MapSectionTable<'rom> {
    /// Returns a [`MapSectionTable`] if it could read it from the ROM.
    pub fn init(rom: &'rom mut Rom) -> Result<Self, TableInitError> {
        if rom.refs.mapsec_name_table.is_none() {
            let mapsec_table = init_mapsec_table(rom)?;
            rom.refs.mapsec_name_table = Some(mapsec_table);
        }

        Ok(MapSectionTable { rom })
    }

    /// Returns a [`MapSectionDump`] with all the map section names.
    pub fn dump_names(&self) -> Result<MapSectionDump, MapSectionError> {
        let mapsec_table = self.get_name_table()?;
        let mut names = Vec::new();

        let (shift, scale) = self.get_name_shift_and_scale()?;

        for i in 0..mapsec_table.size {
            let in_table_offset = mapsec_table.offset + i * scale + shift;
            let text_offset = self.rom.read_ptr(in_table_offset);

            if text_offset.is_err() {
                names.push(None);
                continue;
            }

            if let Some(name) = self.rom.read_text(text_offset.unwrap()).ok() {
                names.push(Some(name.to_string()));
            } else {
                names.push(None);
            }
        }

        Ok(MapSectionDump {
            start_index: get_mapsec_start(self.rom).ok_or(MapSectionError::NoMapsecBounds)? as u8,
            none_index: get_mapsec_none(self.rom).ok_or(MapSectionError::NoMapsecBounds)? as u8,
            names,
        })
    }

    /// Get the first mapsec index.
    pub fn get_start_index(&self) -> Result<u8, MapSectionError> {
        Ok(get_mapsec_start(self.rom).ok_or(MapSectionError::NoMapsecBounds)? as u8)
    }

    fn get_name_shift_and_scale(&self) -> Result<(usize, usize), MapSectionError> {
        match self.rom.rom_type {
            RomType::FireRed | RomType::LeafGreen => Ok((0, 4)),
            RomType::Ruby | RomType::Sapphire | RomType::Emerald => Ok((4, 8)),
            // _ => Err(MapSectionError::NoShiftAndScale),
        }
    }

    /// Returns the [`TablePointer`] of the map section name table.
    fn get_name_table(&self) -> Result<&TablePointer, MapSectionError> {
        self.rom
            .refs
            .mapsec_name_table
            .as_ref()
            .ok_or(MapSectionError::NotInitialized)
    }
}

impl Rom {
    /// Return the [`MapSectionTable`] for this ROM.
    pub fn mapsec(&mut self) -> MapSectionTable {
        MapSectionTable::init(self).unwrap_or_else(|_| {
            panic!("You have to initialize the map sections table before calling rom.mapsec()!")
        })
    }
}

fn init_mapsec_table(rom: &Rom) -> Result<TablePointer, TableInitError> {
    // Find the offset depending on the ROM type
    let offset = match rom.rom_type {
        RomType::FireRed => 0x3F1CAC,
        RomType::LeafGreen => 0x3F1AE8,
        RomType::Ruby => 0x3E73C4,
        RomType::Sapphire => 0x3E741C,
        RomType::Emerald => 0x5A147C,
    };

    let size = get_mapsec_none(rom).ok_or(TableInitError::NotImplemented)?
        - get_mapsec_start(rom).ok_or(TableInitError::NotImplemented)?;
    let references = rom.find_references(offset);

    Ok(TablePointer {
        offset,
        size,
        references,
    })
}

fn get_mapsec_start(rom: &Rom) -> Option<usize> {
    let data_offset = match rom.rom_type {
        RomType::FireRed => 0xC3CA0,
        RomType::LeafGreen => 0xC3C74,
        RomType::Ruby | RomType::Sapphire => 0xFBF2C,
        RomType::Emerald => 0x1244E4,
    };

    Some(rom.read_byte(data_offset) as usize)
}

fn get_mapsec_none(rom: &Rom) -> Option<usize> {
    let data_offset = match rom.rom_type {
        RomType::FireRed => 0xC0BE6,
        RomType::LeafGreen => 0xC0B06,
        // Cannot read the value from the ROM, so we return the default
        RomType::Ruby | RomType::Sapphire => return Some(0x58),
        RomType::Emerald => 0x124584,
    };

    Some(rom.read_byte(data_offset) as usize)
}
