use std::collections::{HashMap, HashSet};

use crate::{
    maps::tileset::TilesetHeader,
    refs::{TableInitError, TablePointer},
    rom::Rom,
};

use self::layout::MapLayout;

pub mod header;
pub mod layout;
pub mod tileset;

impl Rom {
    /// Initializes the map groups table and the map groups list.
    pub fn init_map(&mut self) -> Result<(), TableInitError> {
        if self.refs.map_groups.is_none() || self.refs.map_groups_list.is_none() {
            let (map_groups_table, map_groups) = get_map_groups_table(self)?;
            self.refs.map_groups = Some(map_groups_table);
            self.refs.map_groups_list = Some(map_groups);
        }

        if self.refs.map_layouts_table.is_none() {
            let map_layouts_table = get_map_layouts_table(self)?;
            self.refs.map_layouts_table = Some(map_layouts_table);
        }

        if self.refs.tilesets_table.is_none() {
            let map_layouts_table = self.refs.map_layouts_table.as_ref().unwrap();
            let tilesets_data = get_tilesets_data(self, &map_layouts_table)?;
            self.refs.tilesets_table = Some(tilesets_data);
        }

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

/// Reads the table pointer to the map layouts table.
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

/// Reads the tilesets data from the map layouts table.
fn get_tilesets_data(
    rom: &Rom,
    layouts_table: &TablePointer,
) -> Result<HashMap<usize, (usize, bool)>, TableInitError> {
    let mut tileset: HashSet<(u32, bool)> = HashSet::new();

    println!("Layouts table size: {}", layouts_table.size);

    // Read all the layouts to extrapolate all tileset numbers
    for i in 0..layouts_table.size {
        let offset = layouts_table.offset + i * 4;
        match rom.read_ptr(offset) {
            Ok(ptr) => {
                let layout: MapLayout = rom.read(ptr).unwrap();
                if let Some(primary) = layout.primary_tileset.offset() {
                    tileset.insert((primary, false));
                }
                if let Some(secondary) = layout.secondary_tileset.offset() {
                    tileset.insert((secondary, true));
                }
            }
            Err(_) => continue,
        }
    }

    let mut tilesets_data: HashMap<usize, (usize, bool)> = HashMap::new();

    // For each tileset you found
    for (tileset_offset, is_secondary) in tileset {
        // Read the tileset data
        let tileset_data: TilesetHeader = rom
            .read(tileset_offset as usize)
            .map_err(|_| TableInitError::TableGoesOutOfBounds)?;

        // Make sure both the blocks offset and the behaviors offset are valid
        if let Some(blocks_offset) = tileset_data.metatiles.offset() {
            if let Some(behaviors_offset) = tileset_data.behaviors.offset() {
                // These should be adjacent to each other
                let size = (behaviors_offset - blocks_offset) >> 4;

                if size <= 0 {
                    println!(
                        "[Warning] Cannot compute tileset size for {:X}, using default",
                        tileset_offset
                    );
                    match rom.get_maximum_tileset_size() {
                        Ok((primary_lim, secondary_lim)) => {
                            if is_secondary {
                                tilesets_data
                                    .insert(tileset_offset as usize, (secondary_lim, true));
                            } else {
                                tilesets_data.insert(tileset_offset as usize, (primary_lim, false));
                            }
                        }
                        Err(_) => continue,
                    }
                } else {
                    tilesets_data.insert(tileset_offset as usize, (size as usize, is_secondary));
                }
            }
        }
    }

    Ok(tilesets_data)
}
