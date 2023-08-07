use std::fmt::Display;

use gba_macro::gba_struct;
use gba_types::{pointers::PointedData, GBAIOError, GBAType};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::{
    refs::{TableInitError, TablePointer},
    rom::{Rom, RomType},
};

gba_struct!(WildMon {
    u8 min_level;
    u8 max_level;
    u16 species;
} CUSTOM_DEBUG);

impl WildMon {
    pub fn new(min_level: u8, max_level: u8, species: u16) -> Self {
        Self {
            min_level,
            max_level,
            species,
        }
    }
}

impl std::fmt::Debug for WildMon {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "WildMon({:>4} Lv. {:>3}-{:<3})",
            self.species, self.min_level, self.max_level
        )
    }
}

impl Display for WildMon {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:<4} Lv. {:>3}-{:<3}",
            self.species, self.min_level, self.max_level
        )
    }
}

gba_struct!(LandMonInfo {
    u8 rate;
    struct WildMon [12]*mons;
});
gba_struct!(WaterMonInfo {
    u8 rate;
    struct WildMon [5]*mons;
});
gba_struct!(RockSmashMonInfo {
    u8 rate;
    struct WildMon [5]*mons;
});
gba_struct!(FishingMonInfo {
    u8 rate;
    struct WildMon [10]*mons;
});

gba_struct!(WildMonHeader {
    u8 map_group;
    u8 map_index;
    struct LandMonInfo *land_mons;
    struct WaterMonInfo *water_mons;
    struct RockSmashMonInfo *rock_smash_mons;
    struct FishingMonInfo *fishing_mons;
});

impl WildMonHeader {
    /// Create a new empty wild mon header with the given map group and index.
    fn new(map_group: u8, map_index: u8) -> Self {
        Self {
            map_group,
            map_index,
            land_mons: PointedData::Null,
            water_mons: PointedData::Null,
            rock_smash_mons: PointedData::Null,
            fishing_mons: PointedData::Null,
        }
    }

    /// Updates the given header with the abstract data in the other,
    /// allocating new pointers or reutilizing the same ones if possible.
    fn update_with(&mut self, rom: &mut Rom, other: WildEncounters) -> Result<(), WildError> {
        // The group and index don't change

        macro_rules! update_terrain {
            ($inmons:ident, $other_data:expr, $strname:ident, $count:literal) => {
                self.$inmons = match (self.$inmons.data(), $other_data) {
                    // If there is no data attached to the header for this kind of encounter,
                    // and we don't want to add any, just return the null pointer.
                    (None, None) => PointedData::Null,
                    // If there is no data attached to the header for this kind of encounter,
                    // but we want to add some, allocate a new pointer.
                    (None, Some(data)) => {
                        const COUNT: usize = $count;

                        if data.mons.len() != COUNT {
                            return Err(WildError::WrongEncountersCount(data.mons.len(), COUNT));
                        }

                        // Allocate enough space for the data table and the pointer to that data
                        let table_offset = rom
                            .find_free_space(COUNT * WildMon::SIZE + LandMonInfo::SIZE, 4)
                            .ok_or(WildError::RepointingError)?;

                        // Try to convert the data into an array
                        let data_table: [WildMon; COUNT] = data.mons.try_into().unwrap();

                        // Create the struct to allocate
                        let mons = $strname {
                            rate: data.rate,
                            mons: PointedData::Valid(table_offset as u32, data_table),
                        };

                        // Write the struct to the ROM
                        let struct_offset = table_offset + COUNT * WildMon::SIZE;
                        rom.write(struct_offset, mons)?;

                        PointedData::NoData(struct_offset as u32)
                    }
                    // If there is no data to write but there is some attached to the header,
                    // clear the pointer.
                    (Some(info), None) => {
                        const COUNT: usize = $count;

                        // Delete the struct immediately since we have a copy of it
                        let struct_offset = self.$inmons.offset().unwrap();
                        rom.clear(struct_offset, $strname::SIZE)?;

                        // Delete the data referenced by the struct
                        if let Some(offset) = info.mons.offset() {
                            // If the data is a valid pointer, clear it
                            rom.clear(offset, COUNT * WildMon::SIZE)?;
                        }

                        PointedData::Null
                    }
                    // If there is data attached to the header and we want to add some,
                    // reuse the same pointer.
                    (Some(_), Some(data)) => {
                        const COUNT: usize = $count;

                        if data.mons.len() != COUNT {
                            return Err(WildError::WrongEncountersCount(data.mons.len(), COUNT));
                        }

                        // TODO Review this assumption
                        // Assumption: if there is an info struct allocated, then there is contains
                        // a valid pointer to the data table.
                        let data_table: [WildMon; COUNT] = data.mons.try_into().unwrap();

                        // Create the new table offset
                        assert!(self.$inmons.offset().is_some());
                        let table_offset = self.$inmons.offset().unwrap() as u32;
                        let table = PointedData::Valid(table_offset, data_table);

                        match self.$inmons {
                            PointedData::Valid(old_offset, _) => PointedData::Valid(
                                old_offset,
                                $strname {
                                    rate: data.rate,
                                    mons: table,
                                },
                            ),
                            _ => unreachable!(),
                        }
                    }
                };
            };
        }

        update_terrain!(land_mons, other.land, LandMonInfo, 12);
        update_terrain!(water_mons, other.water, WaterMonInfo, 5);
        update_terrain!(rock_smash_mons, other.rock_smash, RockSmashMonInfo, 5);
        update_terrain!(fishing_mons, other.fishing, FishingMonInfo, 10);

        Ok(())
    }

    /// If there is some data attached to the header, clears it.
    fn clear(self, rom: &mut Rom) -> Result<(), GBAIOError> {
        macro_rules! clear_terrain {
            ($inmons:ident, $strname:ident, $count:literal) => {
                match self.$inmons {
                    PointedData::Valid(offset, data) => {
                        rom.clear(offset as usize, $strname::SIZE)?;
                        // Clear the data table
                        if let PointedData::Valid(offset, _) = &data.mons {
                            rom.clear(*offset as usize, $count * WildMon::SIZE)?;
                        }
                    }
                    _ => (),
                }
            };
        }

        clear_terrain!(land_mons, LandMonInfo, 12);
        clear_terrain!(water_mons, WaterMonInfo, 5);
        clear_terrain!(rock_smash_mons, RockSmashMonInfo, 5);
        clear_terrain!(fishing_mons, FishingMonInfo, 10);

        Ok(())
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct EncountersData {
    pub rate: u8,
    pub mons: Vec<WildMon>,
}

/// Special struct for reading and writing wild mon data.
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct WildEncounters {
    pub land: Option<EncountersData>,
    pub water: Option<EncountersData>,
    pub rock_smash: Option<EncountersData>,
    pub fishing: Option<EncountersData>,
}

impl Display for WildEncounters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut has_data = false;

        macro_rules! write_data {
            ($part:ident, $string:literal) => {
                if let Some(data) = &self.$part {
                    has_data = true;
                    writeln!(f, "{} (rate {}): ", $string, data.rate)?;
                    for mon in &data.mons {
                        writeln!(f, "  - {}", mon)?;
                    }
                }
            };
        }
        write_data!(land, "Land");
        write_data!(water, "Water");
        write_data!(rock_smash, "Rock Smash");
        write_data!(fishing, "Fishing");
        if !has_data {
            writeln!(f, "No encounters")?;
        }
        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum WildError {
    #[error("The wild encounters table is not initialized")]
    TableNotInitialized,
    #[error("Repointing error")]
    RepointingError,

    #[error("Wrong encounters count (expected {1}, got {0})")]
    WrongEncountersCount(usize, usize),

    #[error("IO error")]
    IoError(#[from] gba_types::GBAIOError),
}

pub struct WildsTable<'rom> {
    rom: &'rom mut Rom,
}

impl<'rom> WildsTable<'rom> {
    /// Initializes the map headers table.
    pub fn init(rom: &'rom mut Rom) -> Result<Self, TableInitError> {
        if rom.refs.wilds_table.is_none() {
            let wilds_table = get_wilds_table(rom)?;
            rom.refs.wilds_table = Some(wilds_table);
        }
        Ok(Self { rom })
    }

    // ANCHOR Data
    /// Reads the wild mon data for a given map into a [WildMonHeader] struct.
    pub fn read_encounters(
        &self,
        map_group: u8,
        map_index: u8,
    ) -> Result<Option<WildEncounters>, WildError> {
        // Find the header index for the given map
        let header_index = self.find_map_index(map_group, map_index)?;

        // If there is no header, return None
        let header = match header_index {
            Some(index) => self.read_header(index)?,
            None => return Ok(None),
        };

        macro_rules! clone_encounters {
            ($encounters:ident) => {
                match header.$encounters.data() {
                    None => None,
                    Some(info) => match info.mons.data() {
                        None => None,
                        Some(mons) => Some(EncountersData {
                            rate: info.rate,
                            mons: mons.to_vec(),
                        }),
                    },
                }
            };
        }

        // Read the encounters of each type
        Ok(Some(WildEncounters {
            land: clone_encounters!(land_mons),
            water: clone_encounters!(water_mons),
            rock_smash: clone_encounters!(rock_smash_mons),
            fishing: clone_encounters!(fishing_mons),
        }))
    }

    /// Writes or clears the wild mon data for a given map.
    pub fn write_encounters(
        &mut self,
        map_group: u8,
        map_index: u8,
        data: Option<WildEncounters>,
    ) -> Result<(), WildError> {
        // Check the user input and the current header
        match (self.find_map_index(map_group, map_index)?, data) {
            // If there is no header and no data, do nothing
            (None, None) => Ok(()),
            // If there is no header but there is data, create a new header
            // and write the data to it
            (None, Some(data)) => {
                let index = self.create_header(map_group, map_index)?;
                self.update_header(index, data)
            }
            // If there is a header but no data, clear the header
            (Some(index), None) => {
                // Read the header to clear its content
                let header = self.read_header(index)?;
                header.clear(self.rom)?;

                // Remove the header from the table
                self.remove_header(index)
            }
            // If there was a header and there is data to write, move
            // the data to write into the header and rewrite it.
            (Some(index), Some(data)) => self.update_header(index, data),
        }
    }

    // ANCHOR Headers
    /// Get wild mon info at the given index
    fn read_header(&self, index: usize) -> Result<WildMonHeader, WildError> {
        // Get the offset of the header
        let table = self.get_table()?;
        let offset = table.offset + index * WildMonHeader::SIZE;

        Ok(self.rom.read(offset)?)
    }

    /// Remove wild mon header given its index in the table
    ///
    /// The deletion of all data refenced by the header is not handled by this function.
    fn remove_header(&mut self, index: usize) -> Result<(), WildError> {
        // Copy everything from the index to the end
        let table = self.get_table()?;

        // To delete 3.2, copy the following slice
        // [3.1 ...][3.2 ...][3.3 ...][3.4 ...][NULL]
        //          ^        ^                 ^
        //          |        start_offset      end_offset
        //          New starting offset
        let start_offset = table.offset + (index + 1) * WildMonHeader::SIZE;
        let end_offset = table.offset + (table.size + 1) * WildMonHeader::SIZE;

        // Copy the slice to move back
        let slice_copy = self.rom.data[start_offset..end_offset].to_vec();

        // Write the copy back to the rom
        self.rom.data[start_offset - WildMonHeader::SIZE..end_offset - WildMonHeader::SIZE]
            .copy_from_slice(&slice_copy);

        // The result will be
        // [3.1 ...][3.3 ...][3.4 ...][NULL][NULL]

        Ok(())
    }

    /// Creates a new header and returns its index in the table.
    ///
    /// Assumes there is no wilds header for the `group.index` map header.
    fn create_header(&mut self, map_group: u8, map_index: u8) -> Result<usize, WildError> {
        let table = self.get_table()?;
        let capacity = table.size;

        // `table.size` is the capacity. We have to determine the current size
        // by counting the number of non-empty headers before the first empty one.

        // Consider this case:
        // [3.1 ...][3.2 ...][3.3 ...][NULL]
        // ^        ^        ^        ^
        // 0        1        2        3
        // size = 3, capacity = 3
        // However, the last valid index is 2 and the actual size is 4 * WildMonHeader::SIZE

        // A case after some deletion will be
        // [3.1 ...][NULL][NULL][NULL]
        // ^        ^     ^     ^
        // 0        1     2     3
        // size = 1, capacity = 3

        let mut last_index = table.size;
        loop {
            // Check if this header is empty
            let map_id = self
                .rom
                .read::<u16>(table.offset + last_index * WildMonHeader::SIZE)?;

            if map_id == 0xFFFF {
                last_index -= 1;
            } else {
                break;
            }
        }
        let new_index = last_index + 1;

        // If the size is equal to the capacity, we have to increase the capacity
        if new_index == capacity {
            self.increase_capacity(new_index + 1)?;
        }

        // Get the new table offset
        let table_offset = self.get_table()?.offset;

        // In any case, save an header at the index `size`
        let new_header_offset = table_offset + new_index * WildMonHeader::SIZE;
        let new_header = WildMonHeader::new(map_group, map_index);
        self.rom.write(new_header_offset, new_header)?;

        Ok(new_index)
    }

    /// Updates an existing header with the given data.
    fn update_header(&mut self, index: usize, data: WildEncounters) -> Result<(), WildError> {
        // Read the header to update
        let mut header = self.read_header(index)?;
        // Replace the data in the header
        header.update_with(self.rom, data)?;

        // Get the offset to write the header
        let header_offset = self.get_table()?.offset + index * WildMonHeader::SIZE;
        Ok(self.rom.write(header_offset, header)?)
    }

    // ANCHOR Internal
    /// Returns the index for the given map in the wild mon info table.
    fn find_map_index(&self, map_group: u8, map_index: u8) -> Result<Option<usize>, WildError> {
        // Compose the group.index into an u16
        let map_id = (map_index as u16) << 8 | map_group as u16;

        // Find the index of the map in the table
        let table = self.get_table()?;
        let mut i = 0;
        loop {
            let curr_map_id = self
                .rom
                .read::<u16>(table.offset + i * WildMonHeader::SIZE)?;

            if curr_map_id == map_id {
                return Ok(Some(i));
            } else if curr_map_id == 0xFFFF || i >= table.size {
                return Ok(None);
            }

            i += 1;
        }
    }

    /// Resize the wild mon info table to fit the given number of entries.
    ///
    /// Reverses an extra `WildMonHeader::SIZE` bytes for the end block.
    fn increase_capacity(&mut self, new_size: usize) -> Result<(), WildError> {
        let table = self.get_table()?.clone();
        let old_byte_size = (table.size + 1) * WildMonHeader::SIZE;

        // Make a copy of the table and clear it
        let table_copy = self.rom.data[table.offset..table.offset + old_byte_size].to_vec();
        self.rom
            .clear(table.offset, old_byte_size)
            .map_err(|_| WildError::RepointingError)?;

        let new_byte_size = (new_size + 1) * WildMonHeader::SIZE;

        // Find a new location for the table
        let new_offset = self
            .rom
            .find_free_space(new_byte_size, 4)
            .ok_or(WildError::RepointingError)?;
        // Copy the table back to the new location
        self.rom.data[new_offset..new_offset + old_byte_size].copy_from_slice(&table_copy);

        // Fill the end block with 0s (after the 0xFFFF)
        let last_header_offset = new_offset + new_size * WildMonHeader::SIZE;
        self.rom.data[last_header_offset + 2..last_header_offset + WildMonHeader::SIZE - 2].fill(0);

        // Update the references and the table
        let new_pointer = table
            .update(self.rom, new_offset, new_size)
            .map_err(|_| WildError::RepointingError)?;
        self.rom.refs.wilds_table = Some(new_pointer);

        Ok(())
    }

    /// Returns a reference to the wild mon info table.
    fn get_table(&self) -> Result<&TablePointer, WildError> {
        self.rom
            .refs
            .wilds_table
            .as_ref()
            .ok_or(WildError::TableNotInitialized)
    }

    pub fn debug_print(&self) -> Result<(), WildError> {
        let table = self.get_table()?;

        println!("Wild mons table:");
        println!("  Offset: 0x{:X}", table.offset);
        println!("  Size: {}", table.size);

        for i in 0..table.size + 1 {
            // Read the header
            let header = self
                .rom
                .read::<WildMonHeader>(table.offset + i * WildMonHeader::SIZE)?;

            if header.map_group == 0xFF && header.map_index == 0xFF {
                print!("[NULL]");
            } else {
                print!("[{}.{}]", header.map_group, header.map_index);
            }
        }
        println!();

        Ok(())
    }
}

impl Rom {
    /// Return the [`MapSectionTable`] for this ROM.
    pub fn wilds(&mut self) -> WildsTable {
        WildsTable::init(self).unwrap_or_else(|_| {
            panic!("You have to initialize the wilds table before calling rom.wilds()!")
        })
    }
}

fn get_wilds_table(rom: &mut Rom) -> Result<TablePointer, TableInitError> {
    let table_offset = match rom.rom_type {
        RomType::FireRed => 0x3C9CB8,
        RomType::LeafGreen => 0x3C9AF4,
        RomType::Ruby => 0x39D454,
        RomType::Sapphire => 0x39D29C,
        RomType::Emerald => 0x552D48,
    };

    // The table ends when you encounter a 0xFFFF
    let mut size = 0;
    loop {
        let map_id = rom
            .read::<u16>(table_offset + size * WildMonHeader::SIZE)
            .map_err(|_| TableInitError::TableGoesOutOfBounds)?;

        if map_id == 0xFFFF {
            break;
        }

        size += 1;
    }

    Ok(TablePointer {
        offset: table_offset,
        size,
        references: rom.find_references(table_offset),
    })
}
