use gba_macro::gba_struct;

use crate::rom::Rom;

gba_struct!(Tileset {
    u8 is_compressed;
    u8 is_secondary;
    void* graphics;
    void* palette;
    void* blocks;
    void* animations;
    void* behaviors;
});

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

#[derive(Debug)]
pub enum LayoutError {
    LayoutTableNotLoaded,

    IndicesStartAtOne,
    InvalidIndex(u16),
    
    InvalidOffset(u32),

    ReadError(gba_types::GBAIOError),
}

impl Rom {
    /// Reads the map layout at the given index.
    pub fn read_map_layout(&self, index: u16) -> Result<MapLayout, LayoutError> {
        // Get the map layouts table
        let layouts = self
            .refs
            .get_map_layouts_table()
            .ok_or_else(|| LayoutError::LayoutTableNotLoaded)?;

        if index == 0 {
            return Err(LayoutError::IndicesStartAtOne);
        }

        if index as usize >= layouts.size {
            return Err(LayoutError::InvalidIndex(index));
        }

        // Get the map layout
        let offset = layouts.offset + (index as usize - 1) * 4;
        let offset = self
            .read_ptr(offset)
            .map_err(|_| LayoutError::InvalidOffset(offset as u32))?;

        self.read::<MapLayout>(offset)
            .map_err(LayoutError::ReadError)
    }
}
