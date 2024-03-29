use serde::Serialize;

use gba_macro::gba_struct;
use gba_types::pointers::{Nothing, PointedData};
use gba_types::{GBAIOError, GBAType};
use thiserror::Error;

use crate::refs::{TableInitError, TablePointer};
use crate::rom::{Rom, RomType};

use super::mapgrid::{MapGrid, MapGridError, MapGridMasks};

gba_struct!(EmeraldMapLayout {
    i32 width;
    i32 height;
    void* border;
    void* data;
    void* primary_tileset;
    void* secondary_tileset;
} PRIVATE);

gba_struct!(MapLayout {
    i32 width;
    i32 height;
    void* border;
    void* data;
    void* primary_tileset;
    void* secondary_tileset;
    u8 border_width;
    u8 border_height;
});

impl MapLayout {
    /// Reads the [`MapLayout`] taking into account the game version.
    /// And returning the most complete struct.
    pub fn read(rom: &Rom, offset: usize) -> Result<Self, GBAIOError> {
        let res = match rom.rom_type {
            RomType::FireRed | RomType::LeafGreen => rom.read::<MapLayout>(offset)?,
            RomType::Emerald | RomType::Ruby | RomType::Sapphire => {
                let value = rom.read::<EmeraldMapLayout>(offset)?;
                MapLayout {
                    width: value.width,
                    height: value.height,
                    border: value.border,
                    data: value.data,
                    primary_tileset: value.primary_tileset,
                    secondary_tileset: value.secondary_tileset,
                    border_width: 2,
                    border_height: 2,
                }
            }
        };

        Ok(res)
    }

    /// Writes the [`MapLayout`] taking into account the game version.
    pub fn write(self, rom: &mut Rom, offset: usize) -> Result<(), GBAIOError> {
        match rom.rom_type {
            RomType::FireRed | RomType::LeafGreen => rom.write(offset, self)?,
            RomType::Emerald | RomType::Ruby | RomType::Sapphire => {
                let value = EmeraldMapLayout {
                    width: self.width,
                    height: self.height,
                    border: self.border,
                    data: self.data,
                    primary_tileset: self.primary_tileset,
                    secondary_tileset: self.secondary_tileset,
                };
                rom.write::<EmeraldMapLayout>(offset, value)?;
            }
        };

        Ok(())
    }

    /// Clears the [`MapLayout`] taking into account the game version.
    pub fn clear(rom: &mut Rom, offset: usize) -> Result<(), GBAIOError> {
        rom.clear(offset as usize, MapLayout::size(rom))
    }

    /// Returns the size of the [`MapLayout`] taking into account the game version.
    fn size(rom: &Rom) -> usize {
        match rom.rom_type {
            RomType::FireRed | RomType::LeafGreen => MapLayout::SIZE,
            RomType::Emerald | RomType::Ruby | RomType::Sapphire => EmeraldMapLayout::SIZE,
        }
    }

    /// Checks if the [`MapLayout`] is valid
    pub(crate) fn is_valid(&self) -> bool {
        // For a layout to be valid, it must pass the following checks:
        //  1. The width and height must be greater than 0
        self.width > 0 && self.height > 0
        //  2. The product of the width and height must be less than or equal to 0x400
            && self.width < 0x2800 && self.height < 0x2800 && self.width * self.height <= 0x2800
        //  3. The border size must be greater than 0
            && self.border_width > 0 && self.border_height > 0
        //  4. The border and data pointers must be valid
            && self.border.is_writable() && self.data.is_writable()
        //  5. The tileset pointers must be valid
            && self.primary_tileset.is_writable() && self.secondary_tileset.is_writable()
    }
}

/// Struct for passing around the map layout header
/// and the map and border data.
#[derive(Serialize, Debug)]
pub struct MapLayoutData {
    /// The index of the layout in the table.
    pub index: u16,
    /// The `MapLayout` header.
    pub header: MapLayout,
    /// The map grid data (metatile, elevation and collision)
    pub map_data: MapGrid,
    /// The border grid data (metatile, elevation and collision)
    pub border_data: MapGrid,
}

/// Error type for map layout operations.
#[derive(Debug, Error)]
pub enum LayoutError {
    #[error("Layout table not initialized")]
    LayoutTableNotInitialized,

    #[error("Layout indices start at 1")]
    IndicesStartAtOne,
    #[error("Layout {0} is missing the header")]
    MissingLayout(u16),
    #[error("Layout {0} is out of bounds")]
    IndexOutOfBounds(u16),

    #[error("Invalid layout offset {0:#010x}")]
    InvalidOffset(u32),
    #[error("Invalid map data offset")]
    InvalidMap,

    #[error("Cannot repoint the map layout table")]
    CannotRepointTable,
    #[error("Cannot repoint layout header")]
    CannotRepointHeader,
    #[error("The map layout table is full")]
    TableFull,

    #[error("Cannot get the number of bits per block")]
    CannotGetBitsPerBlock,
    #[error("Map grid error: {0}")]
    MapGridError(#[from] MapGridError),
    #[error("IO Error")]
    IoError(#[from] GBAIOError),
}

/// Table of map layouts. Provides methods for editing the table.
pub struct MapLayoutsTable<'rom> {
    pub rom: &'rom mut Rom,
}

impl<'rom> MapLayoutsTable<'rom> {
    /// Initialize the map layouts table.
    pub fn init(rom: &'rom mut Rom) -> Result<Self, TableInitError> {
        if rom.refs.map_layouts_table.is_none() {
            let map_layouts_table = init_layouts_table(rom)?;
            rom.refs.map_layouts_table = Some(map_layouts_table);
        }

        Ok(Self { rom })
    }

    /// Returns the number of layouts in the table.
    ///
    /// This is just the capacity, not the number of valid layouts.
    pub fn len(&self) -> u16 {
        self.rom.refs.map_layouts_table.as_ref().unwrap().size as u16
    }

    /// Returns the indices of all the existing layouts.
    pub fn dump_valid(&self) -> Result<Vec<u16>, TableInitError> {
        let mut indices = Vec::new();

        for i in 0..self.len() {
            if self.get_header_offset(i).is_ok() {
                indices.push(i);
            }
        }

        Ok(indices)
    }

    // ANCHOR Layout Data
    /// Deletes a map layout from the table.
    pub fn delete_layout(&mut self, index: u16) -> Result<(), LayoutError> {
        // Make sure there is a pointer at that index in the table, then read it
        let header_offset = self.get_header_offset(index)?;

        // Delete the spot from the table
        self.write_offset_to_table(index, None)?;

        // Read the header, then delete it from ROM
        let header = MapLayout::read(self.rom, header_offset)?;

        // Get the correct layout size
        MapLayout::clear(self.rom, header_offset)?;

        // If the map data is valid, delete it
        if let Some(map_offset) = header.data.offset() {
            let map_size = (header.width * header.height * 2) as usize;
            // Delete the map data
            self.rom.clear(map_offset as usize, map_size)?;
        }
        // If the border data is valid, delete it
        if let Some(border_offset) = header.border.offset() {
            let border_size =
                (header.border_width as i32 * header.border_height as i32 * 2) as usize;
            // Delete the border data
            self.rom.clear(border_offset as usize, border_size)?;
        }

        Ok(())
    }

    /// Writes the map layout header at the given index.
    /// and the map and border data, repointing if necessary.
    pub fn write_data(&mut self, mut data: MapLayoutData) -> Result<(), LayoutError> {
        // Make sure the table is big enough to house the new header
        self.increase_table_size(data.index + 1)?;
        // Get the masks for reading MapGrid data
        let masks = MapGridMasks::read_or_default(self.rom);

        // Read the old map and border sizes
        let (map_offset, border_offset) = match self.read_header(data.index) {
            Ok(old) => {
                // If the header exists, read the old map and border sizes
                let old_map_size = (old.width * old.height * 2) as usize;
                let old_border_size = old.border_width as usize * old.border_height as usize * 2;

                // Repoint if necessary
                let old_map_offset_and_size =
                    old.data.offset().map(|offset| (offset, old_map_size));
                let old_border_offset_and_size =
                    old.border.offset().map(|offset| (offset, old_border_size));

                let new_map_offset =
                    data.map_data
                        .repoint(&mut self.rom, old_map_offset_and_size, &masks)?;

                let new_border_offset =
                    data.border_data
                        .repoint(&mut self.rom, old_border_offset_and_size, &masks)?;

                (new_map_offset, new_border_offset)
            }
            Err(_) => {
                let map_offset = data.map_data.repoint(&mut self.rom, None, &masks)?;
                let border_offset = data.border_data.repoint(&mut self.rom, None, &masks)?;

                (map_offset, border_offset)
            }
        };

        // Write the header (with all changes)
        data.header.data = PointedData::Valid(map_offset as u32, Nothing);
        data.header.border = PointedData::Valid(border_offset as u32, Nothing);
        self.write_header(data.index, data.header)
    }

    /// Reads the map layout at the given index and returns the header
    /// and the map and border data.
    pub fn read_data(&self, index: u16) -> Result<MapLayoutData, LayoutError> {
        let layout = self.read_header(index)?;

        let map_offset = layout
            .data
            .offset()
            .ok_or_else(|| LayoutError::InvalidMap)? as usize;

        let border_offset = layout
            .border
            .offset()
            .ok_or_else(|| LayoutError::InvalidMap)? as usize;

        let masks = MapGridMasks::read_or_default(self.rom);
        let map_data = MapGrid::read(
            self.rom,
            map_offset,
            layout.width as usize,
            layout.height as usize,
            &masks,
        )?;
        let border_data = MapGrid::read(
            self.rom,
            border_offset,
            layout.border_width as usize,
            layout.border_height as usize,
            &masks,
        )?;

        Ok(MapLayoutData {
            index,
            header: layout,
            map_data,
            border_data,
        })
    }

    /// Creates a new [`MapLayout`] (together with its data) and returns its index.
    pub fn create_data(
        &mut self,
        tileset1: u32,
        tileset2: u32,
        width: i32,
        height: i32,
    ) -> Result<u16, LayoutError> {
        // Get the index
        let index = self.get_first_free_index()?;

        // Create the header
        let header = MapLayout {
            primary_tileset: PointedData::new(tileset1),
            secondary_tileset: PointedData::new(tileset2),
            width,
            height,
            border_width: 2,
            border_height: 2,
            data: PointedData::Null,
            border: PointedData::Null,
        };

        // Create a new map and border data
        let map_data = MapGrid::new(width as usize, height as usize);
        let border_data = MapGrid::new(2, 2);

        // Write the data (finds offset for new data)
        let data = MapLayoutData {
            index,
            header,
            map_data,
            border_data,
        };

        // Try and write the layout to ROM.
        self.write_data(data)?;

        Ok(index)
    }

    // ANCHOR Headers
    /// Reads the map layout header at the given index.
    pub(super) fn read_header(&self, index: u16) -> Result<MapLayout, LayoutError> {
        let offset = self.get_header_offset(index)?;
        Ok(MapLayout::read(self.rom, offset)?)
    }

    /// Writes the map layout header at the given index.
    ///
    /// Assumes the index location can be directly written to, because the table
    /// has already been extended if necessary.
    pub fn write_header(&mut self, index: u16, header: MapLayout) -> Result<(), LayoutError> {
        // Get the offset to which the header will be written
        let offset = match self.get_header_offset(index) {
            Ok(offset) => offset,
            // If the offset is invalid, find new space for the header
            Err(LayoutError::InvalidOffset(_)) | Err(LayoutError::MissingLayout(_)) => self
                .rom
                .find_free_space(MapLayout::size(self.rom), 4)
                .ok_or_else(|| LayoutError::CannotRepointHeader)?,
            // Any other error is returned
            Err(err) => return Err(err),
        };

        // Write the offset to the table
        self.write_offset_to_table(index, Some(offset))?;

        // Write the header itself
        Ok(header.write(self.rom, offset)?)
    }

    // ANCHOR Header offset
    /// Returns the header offset given the index.
    pub fn get_header_offset(&self, index: u16) -> Result<usize, LayoutError> {
        if index == 0 {
            return Err(LayoutError::IndicesStartAtOne);
        }
        // Get the map layouts table
        let layouts = self.get_table()?;

        if index as usize >= layouts.size {
            return Err(LayoutError::IndexOutOfBounds(index));
        }

        // Get the map layout
        let offset = layouts.offset + (index as usize - 1) * 4;
        if self.rom.read::<u32>(offset)? == 0 {
            return Err(LayoutError::MissingLayout(index));
        }
        Ok(self.rom.read_ptr(offset)?)
    }

    /// Returns the first free index in the table
    fn get_first_free_index(&self) -> Result<u16, LayoutError> {
        let table = self.get_table()?;

        // If there is no more space for anything, return an error
        if self.len() == 65535 {
            return Err(LayoutError::TableFull);
        }

        for i in 1..=self.len() {
            let offset = table.offset + (i as usize - 1) * 4;
            if self.rom.read::<u32>(offset)? == 0 {
                return Ok(i);
            }
        }

        Ok(self.len() + 1)
    }

    /// Grows the map layouts table to the given size.
    /// If the table is already bigger than the given size, nothing happens.
    fn increase_table_size(&mut self, new_size: u16) -> Result<(), LayoutError> {
        let table = self.get_table()?;

        if new_size <= table.size as u16 {
            return Ok(());
        }

        // Find free space for the new table
        let new_offset = self
            .rom
            .find_free_space(new_size as usize * 4, 4)
            .ok_or_else(|| LayoutError::CannotRepointTable)?;

        // Make a copy of the old table
        let old_table =
            &self.rom.data[table.offset..table.offset + table.size as usize * 4].to_owned();

        // Update the table pointer
        let old_pointer = self.get_table()?.clone();
        let new_pointer = old_pointer
            .update(&mut self.rom, new_offset, new_size as usize)
            .map_err(|_| LayoutError::CannotRepointTable)?;
        self.rom.refs.map_layouts_table = Some(new_pointer);

        // Fill the new table with 0s
        self.rom.data[new_offset..new_offset + new_size as usize * 4].fill(0);
        // Copy the old table to the new one
        self.rom.data[new_offset..new_offset + old_table.len()].copy_from_slice(&old_table);

        Ok(())
    }

    /// Returns the [`TablePointer`] to the map layouts table if already read,
    /// otherwise returns an error.
    pub fn get_table(&self) -> Result<&TablePointer, LayoutError> {
        if let Some(table) = &self.rom.refs.map_layouts_table {
            Ok(table)
        } else {
            Err(LayoutError::LayoutTableNotInitialized)
        }
    }

    /// Writes the offset to the map layouts table at the given index.
    /// If the offset is `None`, writes 0 (NULL) instead.
    fn write_offset_to_table(
        &mut self,
        index: u16,
        offset: Option<usize>,
    ) -> Result<(), LayoutError> {
        let table = self.get_table()?;
        let offset_offset = table.offset + (index as usize - 1) * 4;

        Ok(match offset {
            Some(offset) => {
                // Write the offset to the table
                self.rom.write_ptr(offset_offset, offset)?
            }
            None => {
                // Write 0 to the table
                self.rom.write(offset_offset, 0u32)?
            }
        })
    }
}

impl Rom {
    /// Returns the [`MapLayoutsTable`] struct for this ROM.
    ///
    /// # Panics
    /// If the map layouts table has not been initialized.
    pub fn map_layouts(&mut self) -> MapLayoutsTable {
        MapLayoutsTable::init(self).unwrap_or_else(|_| {
            panic!("You have to initialize the map layouts table before calling rom.map_layouts()!")
        })
    }
}

/// Reads the [`TablePointer`] to the map layouts table.
fn init_layouts_table(rom: &Rom) -> Result<TablePointer, TableInitError> {
    let table_offset = match rom.rom_type {
        RomType::Emerald => 0x481dd4,
        RomType::FireRed => 0x34eb8c,
        RomType::LeafGreen => 0x34eb6c,
        RomType::Ruby => 0x304f18,
        RomType::Sapphire => 0x304ea8,
    };

    // Find the size of the table
    let mut table_size = 0;
    let mut consecutive_nulls = 0;

    // TODO Check if there will ever be the need to check each
    // layout's integrity.

    for i in 0..65536 {
        let offset = table_offset + i * 4;
        let pointer: u32 = rom
            .read(offset)
            .map_err(|_| TableInitError::TableGoesOutOfBounds)?;

        // Skip NULL pointers
        if pointer == 0 {
            consecutive_nulls += 1;
            table_size += 1;
            continue;
        }

        consecutive_nulls = 0;

        // If the pointer is valid, increase the size
        if rom.is_pointer_valid(pointer) {
            table_size += 1;
        } else {
            break;
        }

        // After x consecutive NULL pointers, then we've reached the end of the table
        if consecutive_nulls == 7 {
            break;
        }
    }

    // Find all references to the table
    Ok(TablePointer {
        offset: table_offset,
        size: table_size,
        references: rom.find_references(table_offset),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    // All tests should be run against a clean copy of firered
    fn get_test_rom() -> Rom {
        let mut rom = Rom::load("roms/firered.gba").unwrap();
        MapLayoutsTable::init(&mut rom).unwrap();
        rom
    }

    #[test]
    fn bad_layouts() {
        let mut rom = get_test_rom();
        let table = rom.map_layouts();

        // Indices start at one
        assert!(matches!(
            table.read_header(0),
            Err(LayoutError::IndicesStartAtOne),
        ));

        // Missing layouts should return an error
        for index in [
            22, 23, 29, 38, 39, 40, 41, 42, 43, 44, 45, 56, 58, 59, 60, 61, 76, 175,
        ] {
            assert!(matches!(
                table.read_header(index),
                Err(LayoutError::MissingLayout(_)),
            ));
        }

        // Out of bounds indices should return an error
        assert!(matches!(
            table.read_header(table.len()),
            Err(LayoutError::IndexOutOfBounds(_)),
        ))
    }
}
