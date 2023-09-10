use image::RgbaImage;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use rom_data::{values::RomValueError, Offset, RomData, RomValues};

use crate::{maps::tileset::TilesetPairRenderingData, Rom};

use super::MapLayoutError;

// ANCHOR MapGrid struct
/// Map data to pass to the writer as a matrix of `BlockInfo`
#[derive(Serialize, Deserialize)]
pub struct MapGrid {
    // Metatiles matrix (in row-major order)
    metatiles: Vec<u16>,
    // Movement permissions matrix (in row-major order)
    permissions: Vec<u8>,
    // Width of the map
    width: u16,
    // Height of the map
    height: u16,
}

impl std::fmt::Debug for MapGrid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct Ellipsis;
        impl std::fmt::Debug for Ellipsis {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "...")
            }
        }

        f.debug_struct("MapGrid")
            .field("width", &self.width)
            .field("height", &self.height)
            .field("metatiles", &Ellipsis)
            .field("permissions", &Ellipsis)
            .finish()
    }
}

impl MapGrid {
    pub fn new(width: u16, height: u16) -> MapGrid {
        let size = (width * height) as usize;

        MapGrid {
            metatiles: vec![0; size],
            permissions: vec![0; size],
            width,
            height,
        }
    }

    /// Obtains the metatile ID at the given coordinates.
    #[inline]
    pub fn get_metatile(&self, x: u16, y: u16) -> u16 {
        self.metatiles[(y * self.width + x) as usize]
    }

    /// Reads the blocks data of the given size from the ROM at the
    /// given offset into a [`MapGrid`] struct
    pub fn read(
        rom: &RomData,
        offset: Offset,
        width: u16,
        height: u16,
        masks: &MapGridMasks,
    ) -> Result<MapGrid, MapGridError> {
        if (width + 15) * (height + 14) > 0x2800 {
            return Err(MapGridError::MapAreaTooBig(width, height));
        }

        let size = (width * height) as usize;
        let mut metatiles = Vec::with_capacity(size);
        let mut levels = Vec::with_capacity(size);

        if offset + size * 2 > rom.size() {
            return Err(MapGridError::OutOfBounds);
        }

        for y in 0..height {
            for x in 0..width {
                let index = (y * width + x) as usize;
                // SAFETY: We already made sure that the whole map is in bounds
                let block = rom.read_halfword(offset + index * 2).unwrap();

                // Use the masks to extract everything from the block
                let metatile = block & masks.metatile_id_mask;
                let elevation = (block & masks.elevation_mask) >> masks.elevation_shift;
                let collision = (block & masks.collision_mask) >> masks.collision_shift;

                // Compose the permission bit
                let permission = ((elevation << 1) | collision) as u8;
                metatiles.push(metatile);
                levels.push(permission);
            }
        }

        Ok(MapGrid {
            metatiles,
            permissions: levels,
            width,
            height,
        })
    }

    /// Writes this [`MapGrid`] to the ROM at the given offset.
    pub(crate) fn write(
        &self,
        rom: &mut RomData,
        offset: Offset,
        masks: &MapGridMasks,
    ) -> Result<(), MapGridError> {
        for y in 0..self.height {
            for x in 0..self.width {
                let index = (y * self.width + x) as usize;
                let level = self.permissions[index] as u16;

                let metatile = self.metatiles[index];
                let collision = level & 1;
                let elevation = level >> 1;

                let block = metatile
                    | ((collision << masks.collision_shift) & masks.collision_mask)
                    | ((elevation << masks.elevation_shift) & masks.elevation_mask);

                rom.write_halfword(offset + index * 2, block)
                    .map_err(|_| MapGridError::OutOfBounds)?;
            }
        }
        Ok(())
    }

    /// Returns the size of the map data in bytes.
    pub fn get_byte_size(&self) -> usize {
        self.metatiles.len() * 2
    }

    /// Renders this grid using the given rendering context.
    pub fn render(&self, renderer: &TilesetPairRenderingData) -> RgbaImage {
        let image_width = self.width as u32 * 16;
        let image_height = self.height as u32 * 16;

        // Create an image that is big enough to contain all the output map
        let mut buffer: _ = RgbaImage::new(image_width, image_height);

        for y in 0..self.height {
            for x in 0..self.width {
                // Get the index of the given metatile
                let index = self.get_metatile(x, y) as usize;
                let offx = x as usize * 16;
                let offy = y as usize * 16;

                renderer.draw_metatile(index, &mut buffer, offx, offy);
            }
        }

        buffer
    }
}

// ANCHOR MapGridError enum
#[derive(Debug, Error)]
pub enum MapGridError {
    #[error("Cannot repoint map grid data")]
    CannotRepointMap,
    #[error("The map grid data goes out of bounds")]
    OutOfBounds,
    #[error("Map size is too big, found {0}x{1}")]
    MapAreaTooBig(u16, u16),
}

// ANCHOR Masks
#[derive(Debug, RomValues, Clone, Copy)]
pub struct MapGridMasks {
    /// `METATILE_ID_MASK`
    ///
    /// This is the mask used to get the metatile ID from the map grid.
    #[value(FireRed   0x058e3c 0x058db8 0x058ecc 0x059370 Word)]
    #[value(LeafGreen 0x058e3c 0x058db8 0x058ecc 0x059370 Word)]
    #[value(Ruby      0x0563f8 0x056384 0x056468 0x0567f8 Word)]
    #[value(Sapphire  0x0563fc 0x056388 0x05646c 0x0567fc Word)]
    #[value(Emerald   0x088218 0x0881a4 0x088288 0x0886d4 Word)]
    #[value(FireRed   0x058a58 Word StackedHalfwords)]
    #[value(LeafGreen 0x058a58 Word StackedHalfwords)]
    #[value(Ruby      0x056044 Word StackedHalfwords)]
    #[value(Sapphire  0x056048 Word StackedHalfwords)]
    #[value(Emerald   0x087e60 Word StackedHalfwords)]
    pub metatile_id_mask: u16,

    /// `METATILE_COLLISION_MASK`
    #[value(FireRed   0x058e2e 0x06c5b8 0x058e20 0x058da0 0x058eb0 0x058f00 MovLsl)]
    #[value(LeafGreen 0x058e2e 0x06c5b8 0x058e20 0x058da0 0x058eb0 0x058f00 MovLsl)]
    #[value(Ruby      0x0563e8 0x067c74 0x0563da 0x05636e 0x05644e 0x056486 MovLsl)]
    #[value(Sapphire  0x0563ec 0x067c78 0x0563de 0x056372 0x056452 0x05648a MovLsl)]
    #[value(Emerald   0x088208 0x09b954 0x0881fa 0x08818e 0x08826e 0x0882a6 MovLsl)]
    pub collision_mask: u16,

    /// `METATILE_COLLISION_SHIFT`
    #[value(FireRed   0x058e34 Lsr)]
    #[value(LeafGreen 0x058e34 Lsr)]
    #[value(Ruby      0x0563ee Lsr)]
    #[value(Sapphire  0x0563f2 Lsr)]
    #[value(Emerald   0x08820e Lsr)]
    pub collision_shift: u16,

    /// `METATILE_ELEVATION_MASK`
    #[value(FireRed   0x058fce MovLsl)]
    #[value(LeafGreen 0x058fce MovLsl)]
    #[value(Ruby      0x0564fa MovLsl)]
    #[value(Sapphire  0x0564fe MovLsl)]
    #[value(Emerald   0x08831a MovLsl)]
    #[value(FireRed   0x058fe8 Word Not16)]
    #[value(LeafGreen 0x058fe8 Word Not16)]
    #[value(Ruby      0x056514 Word Not16)]
    #[value(Sapphire  0x056518 Word Not16)]
    #[value(Emerald   0x088334 Word Not16)]
    pub elevation_mask: u16,

    /// `METATILE_ELEVATION_SHIFT`
    #[value(FireRed   0x058dae Lsr)]
    #[value(LeafGreen 0x058dae Lsr)]
    #[value(Ruby      0x05637c Lsr)]
    #[value(Sapphire  0x056380 Lsr)]
    #[value(Emerald   0x08819c Lsr)]
    pub elevation_shift: u16,
}

impl Default for MapGridMasks {
    fn default() -> Self {
        Self {
            metatile_id_mask: 0x03FF,
            collision_mask: 0x0C00,
            elevation_mask: 0xF000,
            collision_shift: 10,
            elevation_shift: 12,
        }
    }
}

impl MapGridMasks {
    /// Obtains the masks for the map grid or returns the default ones.
    pub fn read_or_default(rom: &RomData) -> Self {
        Self::read_values(rom).unwrap_or_default()
    }

    // ANCHOR Patch
    pub fn apply_patch(rom: &mut Rom) -> Result<(), MapGridMasksPatchError> {
        // Try to read the values already there.
        let old = MapGridMasks::read_values(&rom.data)
            .map_err(MapGridMasksPatchError::CannotReadOldData)?;
        if old.collision_shift == 11 {
            Err(MapGridMasksPatchError::PatchAlreadyApplied)?;
        }

        // Read all the layouts that can be read
        let mut read_data = Vec::new();
        for index in rom.dump_map_layouts()? {
            let data = rom.read_map_layout(index)?;
            read_data.push(data);
        }

        // Patched values
        let patched = MapGridMasks {
            metatile_id_mask: 0x07FF,
            collision_mask: 0x0800,
            elevation_mask: 0xF000,
            collision_shift: 11,
            elevation_shift: 12,
        };
        patched
            .write_values(&mut rom.data)
            .map_err(MapGridMasksPatchError::CannotApplyPatch)?;

        // Rewrite all the layouts
        for data in read_data {
            rom.write_map_layout(data)?;
        }

        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum MapGridMasksPatchError {
    #[error("The patch was already applied")]
    PatchAlreadyApplied,
    #[error("Could not read the previous Map Grid Masks: {0}")]
    CannotReadOldData(RomValueError),
    #[error("Error while applything the patch: {0}")]
    CannotApplyPatch(RomValueError),
    #[error("Could not update a layout that was previously read: {0}")]
    CannotUpdateLayout(#[from] MapLayoutError),
}
