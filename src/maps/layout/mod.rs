use image::RgbaImage;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use rom_data::{rom_struct, types::RomPointer, RomIoError};

use crate::{Rom, RomTable};

use super::tileset::{MapTilesetError, TilesetPair, TilesetPairRenderingData};

/// Functions for reading the map grid (metatile, elevation and collision) from ROM.
mod mapgrid;
pub use mapgrid::{MapGrid, MapGridError, MapGridMasks, MapGridMasksPatchError};
/// Implementation of the [`MapLayoutTable`] trait.
mod methods;

// ANCHOR MapLayout struct
rom_struct!(MapLayout {
    i32 width, height;
    void *border;
    void *data;
    void *primary_tileset;
    void *secondary_tileset;

    #[for(base(Ruby, Sapphire, Emerald), default(2))]
    u8 border_width, border_height;
});

impl MapLayout {
    /// Returns whether the read [`MapLayout`] could be valid.
    pub fn is_valid(&self) -> bool {
        // For a layout to be valid, it must pass the following checks:
        //  1. The width and height must be greater than 0
        self.width > 0 && self.height > 0
        //  2. The vmap size limit must be respected ((w + 15) * (h + 14) < 0x2800)
            && self.width < 0x2800 && self.height < 0x2800 // (to avoid overflows)
            && (self.width + 15) * (self.height + 14) < 0x2800
        //  3. The border size must be reasonable (can still be zero)
            && self.border_width < 128 && self.border_height < 128
        //  4. The border and data pointers must be valid
            && self.border.is_valid() && self.data.is_valid()
        //  5. The tileset pointers must be valid
            && self.primary_tileset.is_valid() && self.secondary_tileset.is_valid()
    }

    /// Returns the [`TilesetPair`] for this layout
    pub fn read_tilesets(&self, rom: &Rom) -> Result<TilesetPair, MapTilesetError> {
        // Get the offset of the primary tileset
        let primary_offset = self
            .primary_tileset
            .offset()
            .ok_or(MapTilesetError::InvalidPrimaryOffset)?;

        // Get the offset of the secondary tileset
        let secondary_offset = self
            .secondary_tileset
            .offset()
            .ok_or(MapTilesetError::InvalidSecondaryOffset)?;

        // Read the tilesets
        TilesetPair::read(rom, primary_offset, secondary_offset)
    }
}

// ANCHOR MapLayoutData struct
/// Struct that contains the [`MapLayout`] header along with its index
/// and the map data it references.
#[derive(Debug, Serialize, Deserialize)]
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

impl MapLayoutData {
    /// Writes this [MapLayoutData] to the given ROM.
    pub fn write(self, rom: &mut Rom) -> MapLayoutResult {
        rom.write_map_layout(self)
    }

    /// Reads the tilesets associated with this layout.
    #[inline]
    pub fn read_tilesets(&self, rom: &Rom) -> Result<TilesetPair, MapTilesetError> {
        self.header.read_tilesets(rom)
    }

    /// Renders this layout's map.
    pub fn render_map(&self, renderer: &TilesetPairRenderingData) -> RgbaImage {
        self.map_data.render(renderer)
    }

    /// Renders this layout's border.
    pub fn render_borders(&self, renderer: &TilesetPairRenderingData) -> RgbaImage {
        self.border_data.render(renderer)
    }

    /// Renders the map with this layout's tilesets.
    ///
    /// This is a faster way to render this map to an image.
    ///
    /// If you want to render the borders as well, use this instead:
    /// ```no_run
    /// let renderer = layout.read_tilesets(&rom)?.into_rendering_data();
    /// let map_image = layout.render_map(&renderer);
    /// let bor_image = layout.render_borders(&renderer);
    /// ```
    pub fn load_tilesets_and_render_map(&self, rom: &Rom) -> Result<RgbaImage, MapTilesetError> {
        // Load the tileset rendering context
        let renderer = self.read_tilesets(rom)?.into_rendering_data();
        // Render the map
        Ok(self.render_map(&renderer))
    }
}

/// Helper type for the result of map layout operations.
type MapLayoutResult<T = ()> = Result<T, MapLayoutError>;

// ANCHOR MapLayoutError
/// Error type for map layout operations.
#[derive(Debug, Error)]
pub enum MapLayoutError {
    #[error("Layout table not initialized")]
    NotInitialized,

    #[error("Layout indices start at 1")]
    IndicesStartAtOne,
    #[error("Layout {0} is out of bounds")]
    IndexOutOfBounds(u16),
    #[error("Layout {0} is missing the header")]
    MissingLayout(u16),
    #[error("Invalid map data offset")]
    InvalidMap,
    #[error("The map layout table is full")]
    TableFull,

    #[error("Map grid error: {0}")]
    MapGridError(#[from] MapGridError),
    #[error("IO Error")]
    IoError(#[from] RomIoError),
}

// ANCHOR Init function
/// Initializes the table of map layouts in the ROM if it is not already initialized.
pub fn init_table(rom: &mut Rom) -> Result<(), RomIoError> {
    // If already initialized, return
    if rom.refs.map_layouts.is_some() {
        return Ok(());
    }

    let table = read_table(rom)?;
    rom.refs.map_layouts = Some(table);

    Ok(())
}

/// Reads the table of map layouts from the ROM.
pub(crate) fn read_table(rom: &Rom) -> Result<RomTable, RomIoError> {
    // Get the layout table offset to start from.
    let table_offset = find_layout_table_offset(rom)?;

    RomTable::extract_from(
        table_offset,
        &rom.data,
        |data, offset| {
            let word = data.read_word(offset)?;
            // Accept NULL pointers
            if word == 0 {
                return Ok(true);
            }

            match RomPointer::from_pointer(word, data).offset() {
                Some(offset) => {
                    let header: MapLayout = data.read(offset)?;
                    Ok(header.is_valid())
                }
                None => Ok(false),
            }
        },
        4,
    )
}

/// Find the offset of the layout table in the ROM
fn find_layout_table_offset(rom: &Rom) -> Result<usize, RomIoError> {
    // This is the code before the reference to the layout table
    // This is the greatest common divisor among the rom bases
    // label:     ; thus certainly aligned to 4 bytes
    // 03 48      mov     r0, =(gMapLayouts)
    // 01 39      sub     r1, #1
    // 89 00      lsl     r1, r1, #2
    // 09 18      add     r1, r1, r0
    // 08 68      ldr     r0, [r1, #0]
    // 02 BC      pop     { r1 }
    // 08 47      bx      r1
    // ?? ??      ; padding to align to 4 bytes
    //        gMapLayouts:
    let code_before_reference = [
        0x03, 0x48, 0x01, 0x39, 0x89, 0x00, 0x09, 0x18, 0x08, 0x68, 0x02, 0xBC, 0x08, 0x47,
    ];
    // Find the second part
    let layout_references = rom.data.find_bytes(&code_before_reference);

    if layout_references.is_empty() {
        return Err(RomIoError::ReadingInvalidPointer(0, 0));
    }

    // Note how we add 16 instead of 14, because the reference, being an offset
    // has to be aligned to 4 bytes.
    let layout_reference = layout_references[0] + 16;

    // Find the offset at the reference
    rom.data.read_offset(layout_reference)
}
