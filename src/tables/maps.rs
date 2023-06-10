use crate::{refs::TablePointer, rom::Rom};

#[derive(Debug)]
pub enum MapError {
    NotImplemented,
    MapTableNotLoaded,
    NoMapTable,
    GroupOfBounds,
    IndexOutOfBounds,
    InvalidHeaderIndex,
}

pub enum MapHeader {}

impl Rom {
    /// Gets the map groups table and the list of map groups.
    pub fn load_map_groups(&mut self) -> Result<(), MapError> {
        if self.refs.map_groups.is_some() && self.refs.map_groups_list.is_some() {
            return Ok(());
        }

        let (map_groups_table, map_groups) = get_map_groups_table(self)?;

        self.refs.map_groups = Some(map_groups_table);
        self.refs.map_groups_list = Some(map_groups);

        Ok(())
    }

    pub fn get_map_header(&self, group: u8, index: u8) -> Result<(), MapError> {
        // Get the map groups table
        let (table, groups_vec) = self
            .refs
            .get_map_groups()
            .ok_or_else(|| MapError::MapTableNotLoaded)?;

        if group as usize >= table.size {
            return Err(MapError::GroupOfBounds);
        }

        // Get the map group
        let map_group = &groups_vec[group as usize];
        if index as usize >= map_group.size {
            return Err(MapError::IndexOutOfBounds);
        }

        // Get the map header

        Ok(())
    }
}

/// Reads the table pointer to the map groups table
/// and to each group of map headers.
fn get_map_groups_table(rom: &Rom) -> Result<(TablePointer, Vec<TablePointer>), MapError> {
    use crate::rom::RomType;

    let base_offset: usize = match rom.rom_type {
        RomType::FireRed | RomType::LeafGreen => 0x5524C,
        _ => return Err(MapError::NotImplemented),
    };

    // Read the pointer at the base offset
    let table_offset = rom
        .read_ptr(base_offset)
        .map_err(|_| MapError::NoMapTable)?;

    // Find all the map groups
    let mut map_groups = vec![];

    for i in 0..256 {
        let offset = table_offset + i * 4;

        if let Ok(ptr) = rom.read_ptr(offset) {
            map_groups.push(TablePointer {
                offset: ptr,
                size: 0,
                references: vec![offset],
            })
        } else {
            break;
        }
    }

    // Determine the size of each map group
    for i in 0..map_groups.len() {
        determine_map_group_size(&mut map_groups, i, rom);
    }

    // Create the table pointer
    let table = TablePointer {
        offset: table_offset,
        size: map_groups.len(),
        references: vec![base_offset],
    };

    Ok((table, map_groups))
}

/// Determines the size of a map group.
fn determine_map_group_size(map_groups: &mut Vec<TablePointer>, index: usize, rom: &Rom) {
    // Get the pointer to the start of i-th map group
    let start_offset = map_groups[index].offset;

    // Read up to the size of the table, until it's not a valid pointer
    let mut size = 0;

    for i in 0..256 {
        let curr_offset = start_offset + i * 4;

        // If the current offset is the same as the start offset
        // of any other map group *except* the current one,
        // then we've reached the end of the current map group.
        if map_groups
            .iter()
            .enumerate()
            .any(|(j, map_group)| j != index && map_group.offset == curr_offset)
        {
            break;
        }

        // Keep going as long as you read a valid pointer
        if let Ok(_) = rom.read_ptr(curr_offset) {
            size += 1;
        } else {
            break;
        }
    }

    map_groups[index].size = size;
}
