use gba_macro::gba_struct;
use gba_types::pointers::{Nothing, PointedData};
use gba_types::GBAType;

use crate::refs::{TableInitError, TablePointer};
use crate::rom::Rom;

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

pub type MapData = Vec<Vec<u16>>;

/// Struct for passing around the map layout header
/// and the map and border data.
#[derive(Debug)]
pub struct MapLayoutData {
    pub index: u16,
    pub header: MapLayout,
    pub map_data: MapData,
    pub border_data: MapData,
}

/// Error type for map layout operations.
#[derive(Debug)]
pub enum LayoutError {
    LayoutTableNotInitialized,

    IndicesStartAtOne,
    MissingLayout,
    IndexOutOfBounds(u16),

    InvalidOffset(u32),

    InvalidMap,

    CannotRepointTable,
    CannotRepointMap,
    CannotRepointHeader,

    IoError(gba_types::GBAIOError),
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

    /// Deletes a map layout from the table.
    pub fn delete_layout(&mut self, index: u16) -> Result<(), LayoutError> {
        // Make sure there is a pointer at that index in the table, then read it
        let header_offset = self.get_header_offset(index)?;

        // Delete the spot from the table
        self.write_offset_to_table(index, None)?;

        // Read the header, then delete it from ROM
        let header = self
            .rom
            .read::<MapLayout>(header_offset)
            .map_err(LayoutError::IoError)?;
        self.rom
            .clear(header_offset as usize, MapLayout::SIZE)
            .map_err(LayoutError::IoError)?;

        // Since the header is valid, delete it from ROM
        self.rom
            .clear(header_offset as usize, MapLayout::SIZE)
            .map_err(LayoutError::IoError)?;

        // If the map data is valid, delete it
        if let Some(map_offset) = header.data.offset() {
            let map_size = (header.width * header.height * 2) as usize;
            // Delete the map data
            self.rom
                .clear(map_offset as usize, map_size)
                .map_err(LayoutError::IoError)?;
        }
        // If the border data is valid, delete it
        if let Some(border_offset) = header.border.offset() {
            let border_size =
                (header.border_width as i32 * header.border_height as i32 * 2) as usize;
            // Delete the border data
            self.rom
                .clear(border_offset as usize, border_size)
                .map_err(LayoutError::IoError)?;
        }

        Ok(())
    }

    /// Writes the map layout header at the given index.
    /// and the map and border data, repointing if necessary.
    pub fn write_data(&mut self, mut data: MapLayoutData) -> Result<(), LayoutError> {
        // Make sure the table is big enough to house the new header
        self.increase_table_size(data.index + 1)?;

        // Read the old map and border sizes
        let (map_offset, border_offset) = match self.read_header(data.index) {
            Ok(old) => {
                // If the header exists, read the old map and border sizes
                let old_map_size = old.width * old.height * 2;
                let old_border_size = old.border_width as i32 * old.border_height as i32 * 2;

                // Repoint if necessary
                let old_map_offset = old.data.offset();
                let old_border_offset = old.border.offset();

                let new_map_offset = write_over_map_data(
                    &mut self.rom,
                    old_map_offset,
                    old_map_size as usize,
                    &data.map_data,
                )?;

                let new_border_offset = write_over_map_data(
                    &mut self.rom,
                    old_border_offset,
                    old_border_size as usize,
                    &data.border_data,
                )?;

                (new_map_offset, new_border_offset)
            }
            Err(_) => {
                // Allocate the map data and the border data
                let map_offset = write_new_map_data(self.rom, &data.map_data)?;
                let border_offset = write_new_map_data(self.rom, &data.border_data)?;

                (map_offset, border_offset)
            }
        };

        // Write the header (with all changes)
        data.header.data = PointedData::Valid(map_offset as u32, Nothing);
        data.header.border = PointedData::Valid(border_offset as u32, Nothing);
        self.write_header(data.index, data.header)
    }

    /// Writes the map layout header at the given index.
    ///
    /// Assumes the index location can be directly written to, because the table
    /// has already been extended if necessary.
    fn write_header(&mut self, index: u16, header: MapLayout) -> Result<(), LayoutError> {
        let offset = self.get_header_offset(index);

        // Get the offset to which the header will be written
        let offset = match offset {
            Ok(offset) => offset,
            // If the offset is invalid, find new space for the header
            Err(LayoutError::InvalidOffset(_)) => self
                .rom
                .find_free_space(MapLayout::SIZE, 4)
                .ok_or_else(|| LayoutError::CannotRepointHeader)?,
            // Any other error is returned
            Err(err) => return Err(err),
        };

        // Write the offset to the table
        self.write_offset_to_table(index, Some(offset))?;

        // Write the header itself
        self.rom
            .write::<MapLayout>(offset, header)
            .map_err(LayoutError::IoError)
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

        let map_data = read_map_data(self.rom, map_offset, layout.width, layout.height)?;
        let border_data = read_map_data(
            self.rom,
            border_offset,
            layout.border_width as i32,
            layout.border_height as i32,
        )?;

        Ok(MapLayoutData {
            index,
            header: layout,
            map_data,
            border_data,
        })
    }

    /// Reads the map layout header at the given index.
    fn read_header(&self, index: u16) -> Result<MapLayout, LayoutError> {
        let offset = self.get_header_offset(index)?;

        self.rom
            .read::<MapLayout>(offset)
            .map_err(LayoutError::IoError)
    }

    /// Returns the header offset given the index.
    fn get_header_offset(&self, index: u16) -> Result<usize, LayoutError> {
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
        if self.rom.read::<u32>(offset).map_err(LayoutError::IoError)? == 0 {
            return Err(LayoutError::MissingLayout);
        }
        self.rom.read_ptr(offset).map_err(LayoutError::IoError)
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

        match offset {
            Some(offset) => {
                // Write the offset to the table
                self.rom
                    .write_ptr(offset_offset, offset)
                    .map_err(LayoutError::IoError)
            }
            None => {
                // Write 0 to the table
                self.rom
                    .write(offset_offset, 0u32)
                    .map_err(LayoutError::IoError)
            }
        }
    }
}

/// Reads the [`TablePointer`] to the map layouts table.
fn init_layouts_table(rom: &Rom) -> Result<TablePointer, TableInitError> {
    use crate::rom::RomType;

    // TODO Replace static offset with a mask
    let base_offset: usize = match rom.rom_type {
        RomType::FireRed | RomType::LeafGreen => 0x55194,
        _ => return Err(TableInitError::NotImplemented),
    };

    // Read the pointer at the base offset
    let table_offset = rom
        .read_ptr(base_offset)
        .map_err(|_| TableInitError::InvalidTablePointer)?;

    // Find the size of the table
    let mut table_size = 0;
    let mut consecutive_nulls = 0;

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

    Ok(TablePointer {
        offset: table_offset,
        size: table_size,
        references: vec![base_offset],
    })
}

/// Reads a map data from the ROM, formatting it as a 2D array.
fn read_map_data(
    rom: &Rom,
    offset: usize,
    width: i32,
    height: i32,
) -> Result<MapData, LayoutError> {
    let mut data = Vec::with_capacity(height as usize);

    for y in 0..height {
        let mut row = Vec::with_capacity(width as usize);

        for x in 0..width {
            let offset = offset + (y * width + x) as usize * 2;
            let tile = rom
                .read::<u16>(offset)
                .map_err(|_| LayoutError::InvalidOffset(offset as u32))?;

            row.push(tile);
        }

        data.push(row);
    }

    Ok(data)
}

/// Allocates space for the map data and writes it to the ROM, returning the offset.
fn write_new_map_data(rom: &mut Rom, map: &MapData) -> Result<usize, LayoutError> {
    let size = map.len() * map[0].len() * 2;

    let offset = rom
        .find_free_space(size, 2)
        .ok_or_else(|| LayoutError::CannotRepointMap)?;

    for (y, row) in map.iter().enumerate() {
        for (x, tile) in row.iter().enumerate() {
            let offset = offset + (y * map[0].len() + x) * 2;
            rom.write(offset, *tile).map_err(LayoutError::IoError)?;
        }
    }

    Ok(offset)
}

/// Gets the old and new size of a map and decides whether to repoint it or not.
fn write_over_map_data(
    rom: &mut Rom,
    old_offset: Option<u32>,
    old_size: usize,
    map: &MapData,
) -> Result<usize, LayoutError> {
    if old_offset.is_none() {
        return write_new_map_data(rom, map);
    }

    // Get the new map size
    let new_size = map.len() * map[0].len() * 2;
    // Repoint
    let offset = rom
        .repoint_offset(old_offset.unwrap() as usize, old_size, new_size)
        .ok_or_else(|| LayoutError::CannotRepointMap)?;

    // Write the new map data
    for (y, row) in map.iter().enumerate() {
        for (x, tile) in row.iter().enumerate() {
            let offset = offset + (y * map[0].len() + x) * 2;
            rom.write(offset, *tile).map_err(LayoutError::IoError)?;
        }
    }

    Ok(offset)
}

#[cfg(test)]
mod tests {
    use super::*;

    // All tests should be run against a clean copy of firered
    fn get_test_rom() -> Rom {
        Rom::load("roms/firered.gba").unwrap()
    }

    #[test]
    fn bad_layouts() {
        let mut rom = get_test_rom();
        let table = MapLayoutsTable::init(&mut rom).unwrap();

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
                Err(LayoutError::MissingLayout),
            ));
        }

        // Out of bounds indices should return an error
        assert!(matches!(
            table.read_header(table.len()),
            Err(LayoutError::IndexOutOfBounds(_)),
        ))
    }

    #[test]
    fn read_layout() {
        // Let's start with layout
    }
}
