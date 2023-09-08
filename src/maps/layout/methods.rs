use rom_data::{
    types::{RomPointer, RomSizedType},
    Offset, RomIoError,
};

use crate::{Rom, RomTable};

use super::{
    MapGrid, MapGridMasks, MapLayout, MapLayoutData, MapLayoutError, MapLayoutResult,
    MapLayoutTable,
};

impl MapLayoutTable for Rom {
    fn get_map_layout_offset(&self, index: u16) -> MapLayoutResult<Offset> {
        let pointer = get_map_layout_pointer(self, index)?;
        Ok(self.data.read_offset(pointer)?)
    }

    // ANCHOR Reading functions
    fn read_map_layout_header(&self, index: u16) -> MapLayoutResult<MapLayout> {
        // Get the pointer to the layout
        let pointer = get_map_layout_pointer(self, index)?;
        // If the pointer is NULL, return a missing layout error
        if self.data.read_word(pointer)? == 0 {
            return Err(MapLayoutError::MissingLayout(index));
        }

        // Make sure the layout is not invalid
        let offset = self.data.read_offset(pointer)?;
        // Read the header
        let header: MapLayout = self.data.read(offset)?;
        Ok(header)
    }
    fn read_map_layout(&self, index: u16) -> MapLayoutResult<MapLayoutData> {
        // Read the header
        let header = self.read_map_layout_header(index)?;

        // Get the map and offsets
        let map_offset = header.data.offset().ok_or(MapLayoutError::InvalidMap)?;
        let bor_offset = header.border.offset().ok_or(MapLayoutError::InvalidMap)?;

        // Get the width and height of the map and the borders
        let map_width = header.width as u16;
        let map_height = header.height as u16;
        let bor_width = header.border_width as u16;
        let bor_height = header.border_height as u16;

        // Read the MapGrid masks to pass to the two readers
        let masks = MapGridMasks::read_or_default(&self.data);

        // TODO What happens if any of those are NULL?

        // Read the map grids
        let map_data = MapGrid::read(&self.data, map_offset, map_width, map_height, &masks)?;
        let bor_data = MapGrid::read(&self.data, bor_offset, bor_width, bor_height, &masks)?;

        Ok(MapLayoutData {
            index,
            header,
            map_data,
            border_data: bor_data,
        })
    }

    // ANCHOR Creation
    fn create_map_layout(
        &mut self,
        primary_offset: Offset,
        secondary_offset: Offset,
        width: i32,
        height: i32,
    ) -> MapLayoutResult<u16> {
        // Get a free index where to put this layout
        let index = get_free_index(self)?;
        // Get the pointer at this index
        let pointer = get_map_layout_pointer(self, index)?;

        // Allocate space for everything
        let (header_offset, bor_offset, map_offset) =
            allocate_new_space(self, (width * height) as usize * 2, 8)?;

        // Create and write the header
        self.data.write(
            header_offset,
            MapLayout {
                primary_tileset: RomPointer::new(primary_offset),
                secondary_tileset: RomPointer::new(secondary_offset),
                border: RomPointer::new(bor_offset),
                data: RomPointer::new(map_offset),
                width,
                height,
                border_width: 2,
                border_height: 2,
            },
        )?;

        // Write the header offset in the new space
        self.data.write_offset(pointer, header_offset)?;

        // Return the new index
        Ok(index)
    }

    // ANCHOR Writing functions
    fn write_map_layout_header(&mut self, index: u16, header: MapLayout) -> MapLayoutResult {
        // Get the pointer to the layout
        let pointer = get_map_layout_pointer(self, index)?;
        // Make sure the layout is not invalid
        let offset = self.data.read_offset(pointer)?;
        // Write the header
        self.data.write(offset, header)?;

        Ok(())
    }
    fn write_map_layout(&mut self, data: MapLayoutData) -> MapLayoutResult {
        let index = data.index;

        // If not big enough, this will update the table so that `index`
        // is the last valid index in it.
        resize_table_if_necessary(self, index as usize)?;

        let newh = data.header;
        let new_map_size = (newh.width * newh.height) as usize * 2;
        let new_bor_size = (newh.border_width * newh.border_height) as usize * 2;

        // Re-use the old offsets if possible.
        let (header_offset, bor_offset, map_offset) = match self.read_map_layout_header(index) {
            // If there is an old header, repoint the old offsets to fit the new sizes.
            Ok(oldh) => {
                // Resize or reallocate the border offset
                let old_size = (oldh.border_width * oldh.border_height) as usize * 2;
                let old_offset = oldh.border.offset();
                let bor_offset =
                    repoint_or_find_free_space(self, old_offset, old_size, new_bor_size)?;

                // Resize or repoint the map offset
                let old_offset = oldh.data.offset();
                let old_size = (oldh.width * oldh.height) as usize * 2;
                let map_offset =
                    repoint_or_find_free_space(self, old_offset, old_size, new_map_size)?;

                // Get the header offset
                let pointer = get_map_layout_pointer(&self, index)?;
                let header_offset = self.data.read_offset(pointer)?;

                (header_offset, bor_offset, map_offset)
            }
            // If the old header has an error, we have to allocate
            Err(_) => allocate_new_space(self, new_map_size, new_bor_size)?,
        };

        // Read the masks for the two
        let masks = MapGridMasks::read_or_default(&self.data);

        // Write the map and border data
        data.map_data.write(&mut self.data, map_offset, &masks)?;
        data.border_data.write(&mut self.data, bor_offset, &masks)?;

        // Update the header with the new offsets
        let newh = MapLayout {
            border: RomPointer::new(bor_offset),
            data: RomPointer::new(map_offset),
            ..newh
        };
        // Write the new header to the header_offset
        self.data.write(header_offset, newh)?;
        // Write the header offset to the index pointer
        let pointer = get_map_layout_pointer(&self, index)?;
        self.data.write_offset(pointer, header_offset)?;

        Ok(())
    }

    // ANCHOR Deletion
    fn delete_map_layout(&mut self, index: u16) -> MapLayoutResult {
        // Read the header
        let header = self.read_map_layout_header(index)?;

        // Clear the data if present
        if let Some(data_offset) = header.data.offset() {
            let size = (header.width * header.height) as usize * 2;
            self.data.clear_bytes(data_offset, size)?;
        }

        // Clear the border if present
        if let Some(border_offset) = header.border.offset() {
            let size = (header.border_width * header.border_height) as usize * 2;
            self.data.clear_bytes(border_offset, size)?;
        }

        // Clear the header
        let pointer = get_map_layout_pointer(self, index)?;
        // Read the offset
        let offset = self.data.read_offset(pointer)?;
        // TODO Better clearing interface
        let header_size = MapLayout::get_size(&self.data);
        // Clear the header
        self.data.clear_bytes(offset, header_size)?;
        // Clear the pointer
        self.data.write_word(pointer, 0)?;

        Ok(())
    }

    // ANCHOR Dumping
    fn dump_map_layouts(&self) -> MapLayoutResult<Vec<u16>> {
        let table = get_table(self)?;

        let mut results = Vec::new();
        for index in 1..=table.length {
            // Get the pointer at this position
            let pointer = table.offset + index * 4 - 4;
            // Check if the pointer is valid
            if self.data.read_offset(pointer).is_ok() {
                results.push(index as u16);
            }
        }

        Ok(results)
    }
}

/// Returns the layout table if present.
fn get_table(rom: &Rom) -> MapLayoutResult<&RomTable> {
    match &rom.refs.map_layouts {
        Some(table) => Ok(table),
        None => Err(MapLayoutError::NotInitialized),
    }
}

/// Gets the pointer to the layout offset with the given index in the table.
fn get_map_layout_pointer(rom: &Rom, index: u16) -> MapLayoutResult<Offset> {
    if index == 0 {
        return Err(MapLayoutError::IndicesStartAtOne);
    }
    // Get the table
    let table = get_table(rom)?;
    // Assert that the index is in bounds
    if index > table.length as u16 {
        return Err(MapLayoutError::IndexOutOfBounds(index));
    }

    let pointer = table.offset + (index as usize) * 4 - 4;
    Ok(pointer)
}

/// Gets a free index for a new map layout. Repoints if necessary
fn get_free_index(rom: &mut Rom) -> MapLayoutResult<u16> {
    // Get the table
    let table = get_table(rom)?;
    // Get the length of the table
    let length = table.length;
    // Get the offset of the table
    let offset = table.offset;

    // Look in each space for a null pointer
    for i in 0..length {
        let pointer = rom.data.read_offset(offset + i * 4)?;
        if pointer == 0 {
            return Ok(i as u16 + 1);
        }
    }
    if length == 65535 {
        return Err(MapLayoutError::TableFull);
    }

    // If you had no luck, resize the table.
    let new_length = length + 1;
    resize_table_if_necessary(rom, new_length)?;

    Ok(new_length as u16)
}

/// Resizes the map layout table to the given length (if necessary).
///
/// This means that the last valid index will be `new_length`.
fn resize_table_if_necessary(rom: &mut Rom, new_length: usize) -> MapLayoutResult {
    let table = get_table(rom)?;

    // If the old length is greater than the new length, return
    if new_length <= table.length {
        return Ok(());
    }

    let table = table.clone().simple_resize(&mut rom.data, new_length, 4)?;
    // Update the table
    rom.refs.map_layouts = Some(table);

    Ok(())
}

/// If an old offset is provided, it is repointed, otherwise a new space is found.
fn repoint_or_find_free_space(
    rom: &mut Rom,
    old_offset: Option<Offset>,
    old_size: usize,
    new_size: usize,
) -> Result<Offset, RomIoError> {
    let offset = match old_offset {
        // If there is an old offset, repoint
        Some(offset) => rom.data.repoint_offset(offset, old_size, new_size),
        // Otherwise find free space
        None => rom.data.find_free_space(new_size, 4),
    }?;

    // Allocate the new space
    rom.data.allocate(offset, new_size)?;

    // Return the new offset
    Ok(offset)
}

/// Allocates new space for the layout header, border and map.
fn allocate_new_space(
    rom: &mut Rom,
    map_size: usize,
    border_size: usize,
) -> MapLayoutResult<(Offset, Offset, Offset)> {
    // Allocate space for the header
    let header_size = MapLayout::get_size(&rom.data);
    let header_offset = rom.data.find_free_space(header_size, 4)?;
    rom.data.allocate(header_offset, header_size)?;

    // Allocate contiguous space for the map and border grids
    let size = map_size + border_size;
    let bor_offset = rom.data.find_free_space(size, 4)?;
    let map_offset = bor_offset + border_size;
    rom.data.allocate(bor_offset, size)?;

    Ok((header_offset, bor_offset, map_offset))
}
