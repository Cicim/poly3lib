use gba_macro::gba_struct;

use crate::rom::Rom;

// gba_struct!(EmeraldMapHeader {
//     void *map_layout;
//     void *events;
//     void *map_scripts;
//     void *connections;
//     u16 music;
//     u16 map_layout_id;
//     u8 region_map_section_id;
//     u8 cave;
//     u8 weather;
//     u8 map_type;
//     u8 filler[2];
//     u8 allow_cycling:1;
//     u8 allow_escaping:1;
//     u8 allow_running:1;
//     u8 show_map_name:5;
//     u8 battle_type;
// } PRIVATE);

gba_struct!(MapHeader {
    void *map_layout;  // struct MapLayout *mapLayout;
    void *events;      // struct MapEvents *events;
    void *map_scripts;
    void *connections; // struct MapConnections *connections;
    u16 music;
    u16 map_layout_id;        // Has to correspond with the map layout
    u8 region_map_section_id;
    u8 cave;
    u8 weather;
    u8 map_type;
    u8 biking_allowed;
    u8 allow_escaping:1;
    u8 allow_running:1;
    u8 show_map_name:6;
    i8 floor_num;
    u8 battle_type;
});

#[derive(Debug)]
pub enum MapError {
    MapTableNotLoaded,

    InvalidIndex(u8, u8),
    InvalidOffset(u8, u8, u32),

    ReadError(gba_types::GBAIOError),
}

impl Rom {
    /// Reads the map header at the given group and index.
    pub fn read_map_header(&self, group: u8, index: u8) -> Result<MapHeader, MapError> {
        // Get the map groups table
        let (table, groups_vec) = self
            .refs
            .get_map_groups()
            .ok_or_else(|| MapError::MapTableNotLoaded)?;

        if group as usize >= table.size {
            return Err(MapError::InvalidIndex(group, index));
        }

        // Get the map group
        let map_group = &groups_vec[group as usize];
        if index as usize >= map_group.size {
            return Err(MapError::InvalidIndex(group, index));
        }

        // Get the map header
        let offset = map_group.offset + index as usize * 4;
        let header_offset = self
            .read_ptr(offset)
            .map_err(|_| MapError::InvalidOffset(group, index, offset as u32))?;

        self.read::<MapHeader>(header_offset)
            .map_err(MapError::ReadError)
    }
}
