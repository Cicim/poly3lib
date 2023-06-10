use gba_macro::gba_struct;

use gba_types::colors::GBAPalette;

gba_struct!(Tileset {
    u8 is_compressed;
    u8 is_secondary;
    void* graphics;
    struct GBAPalette[16]* palette;
    void* blocks;
    void* animations;
    void* behaviors;
});

impl Tileset {
    pub fn is_compressed(&self) -> bool {
        self.is_compressed != 0
    }

    pub fn is_secondary(&self) -> bool {
        self.is_secondary != 0
    }
}

