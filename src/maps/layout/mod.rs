use image::RgbaImage;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use rom_data::{rom_struct, Offset, RomBase, RomIoError};

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

// ANCHOR MapLayoutTable trait
/// Importing this trait allows you to read, write and delete map layouts.
pub trait MapLayoutTable {
    /// Reads the [`MapLayout`] with the given index.
    fn read_map_layout_header(&self, index: u16) -> MapLayoutResult<MapLayout>;
    /// Reads the [`MapLayoutData`] (layout header with map and border data)
    /// with the given index.
    fn read_map_layout(&self, index: u16) -> MapLayoutResult<MapLayoutData>;

    /// Creates a new [`MapLayout`] (and its data) and returns its index.
    fn create_map_layout(
        &mut self,
        primary_tileset: Offset,
        secondary_tileset: Offset,
        width: i32,
        height: i32,
    ) -> MapLayoutResult<u16>;

    /// Writes the given [`MapLayout`] at the given index, overwriting the previous
    /// one if present. Never extends the table if the index is out of bounds.
    fn write_map_layout_header(&mut self, index: u16, header: MapLayout) -> MapLayoutResult;
    /// Writes the map [`MapLayoutData`] to the index contained in it.
    ///
    /// Writes the header and map border. If possible, utilizes the same spots
    /// as the layout previously written in its place.
    fn write_map_layout(&mut self, data: MapLayoutData) -> MapLayoutResult;

    /// Deletes the map layout at the given index from the table.
    fn delete_map_layout(&mut self, index: u16) -> MapLayoutResult;

    /// Dumps all the valid map layouts indices to a vector;
    fn dump_map_layouts(&self) -> MapLayoutResult<Vec<u16>>;
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
    // TODO Better way to find the layout table offset?

    // Get the layout table offset to start from.
    let table_offset = match rom.base() {
        RomBase::Emerald => 0x481dd4,
        RomBase::FireRed => 0x34eb8c,
        RomBase::LeafGreen => 0x34eb6c,
        RomBase::Ruby => 0x304f18,
        RomBase::Sapphire => 0x304ea8,
    };

    RomTable::read_table(
        table_offset,
        &rom.data,
        |data, offset| {
            // Accept NULL pointers
            if data.read_word(offset)? == 0 {
                return Ok(true);
            }

            let header_offset = data.read_offset(offset)?;
            let header: MapLayout = data.read(header_offset)?;
            Ok(header.is_valid())
        },
        4,
    )
}
