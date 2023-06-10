use crate::{
    refs::{TableInitError, TablePointer},
    rom::Rom,
};

pub mod header;
pub mod layout;
pub mod tileset;

impl Rom {
    /// Initializes the map groups table and the map groups list.
    pub fn init_map(&mut self) -> Result<(), TableInitError> {
        if self.refs.map_groups.is_some() && self.refs.map_groups_list.is_some() {
            return Ok(());
        }

        let (map_groups_table, map_groups) = get_map_groups_table(self)?;
        let map_layout_table = get_map_layouts_table(self)?;

        self.refs.map_groups = Some(map_groups_table);
        self.refs.map_groups_list = Some(map_groups);
        self.refs.map_layouts_table = Some(map_layout_table);

        Ok(())
    }
}

/// Reads the table pointer to the map groups table
/// and to each group of map headers.
fn get_map_groups_table(rom: &Rom) -> Result<(TablePointer, Vec<TablePointer>), TableInitError> {
    use crate::rom::RomType;

    // ANCHOR Add support for other rom types
    let base_offset: usize = match rom.rom_type {
        RomType::FireRed | RomType::LeafGreen => 0x5524C,
        _ => return Err(TableInitError::NotImplemented),
    };

    // Read the pointer at the base offset
    let table_offset = rom
        .read_ptr(base_offset)
        .map_err(|_| TableInitError::InvalidTablePointer)?;

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
    for gid in 0..map_groups.len() {
        let map_groups: &mut Vec<TablePointer> = &mut map_groups;
        // Get the pointer to the start of i-th map group
        let start_offset = map_groups[gid].offset;

        for mid in 0..256 {
            let curr_ptr_in_table = start_offset + mid * 4;

            // If the current offset is the same as the start offset
            // of any other map group *except* the current one,
            // then we've reached the end of the current map group.
            if map_groups
                .iter()
                .enumerate()
                .any(|(gid2, other_group)| gid != gid2 && other_group.offset == curr_ptr_in_table)
            {
                break;
            }

            // Keep going as long as you read a valid pointer
            if let Ok(_) = rom.read_ptr(curr_ptr_in_table) {
                map_groups[gid].size += 1;
            } else {
                break;
            }
        }
    }

    // Create the table pointer
    let table = TablePointer {
        offset: table_offset,
        size: map_groups.len(),
        references: vec![base_offset],
    };

    Ok((table, map_groups))
}

fn get_map_layouts_table(rom: &Rom) -> Result<TablePointer, TableInitError> {
    use crate::rom::RomType;

    let base_offset: usize = match rom.rom_type {
        RomType::FireRed | RomType::LeafGreen => 0x55194,
        _ => return Err(TableInitError::NotImplemented),
    };

    // Read the pointer at the base offset
    let table_offset = rom
        .read_ptr(base_offset)
        .map_err(|_| TableInitError::InvalidTablePointer)?;

    // Find the size of the table
    let mut table_size = 0usize;
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
