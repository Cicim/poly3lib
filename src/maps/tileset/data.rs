use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use rom_data::{
    types::{RomGraphic, RomPalette, RomSizedType},
    Offset, RomBase, RomData,
};

use crate::Rom;

use super::{MapTilesetError, MapTilesetResult, MetaTile, MetatileAttributes, TilesetHeader};

#[derive(Serialize, Deserialize)]
/// Data that can be exported to a program to give a
/// comprehensive view of a tileset.
pub struct TilesetData {
    /// The tileset's header, as read from the ROM
    pub header: TilesetHeader,
    /// The tileset's graphics (stored as a collection of tiles)
    pub graphics: RomGraphic,
    /// The tileset's palettes
    pub palettes: Vec<RomPalette>,

    /// The metatiles that make up the tileset
    pub metatiles: Vec<MetaTile>,
    /// The attributes for each metatile
    pub attributes: Vec<MetatileAttributes>,
    /*     /// Animation data for the tileset
    ///
    /// This is loaded at a later time explicitly
    pub animations: Option<TilesetAnimationList>, */
}

impl TilesetData {
    /// Read the [`TilesetData`] from the ROM.
    pub fn read(rom: &Rom, offset: Offset) -> Result<Self, MapTilesetError> {
        // Read the header
        let header: TilesetHeader = rom.data.read(offset)?;
        // Get the metatiles count
        let num_metatiles = header.get_metatiles_count();

        // Just to make things shorter
        let rom = &rom.data;

        // Read everything based on whether you can do it.
        let graphics = read_graphics(rom, &header)?;
        let palettes = read_palettes(rom, &header)?;
        let metatiles = read_metatiles(rom, &header, num_metatiles)?;
        let attributes = read_attributes(rom, &header, num_metatiles)?;

        Ok(Self {
            header,
            graphics,
            palettes,
            attributes,
            metatiles,
            // TODO Animations
        })
    }
}

impl Debug for TilesetData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TilesetData")
            .field("header", &self.header)
            .field("graphics", &self.graphics)
            .field("palettes", &self.palettes)
            .field("metatiles", &self.metatiles)
            .field("attributes", &self.attributes)
            .finish()
    }
}

/// Reads the tileset graphics, compressed or uncompressed.
fn read_graphics(rom: &RomData, header: &TilesetHeader) -> MapTilesetResult<RomGraphic> {
    let offset = header
        .graphics
        .offset()
        .ok_or_else(|| MapTilesetError::NoGraphics)?;

    // Get the tile length
    let length = if header.is_compressed {
        None
    } else {
        // REVIEW Is this the most reasonable default?
        Some(256)
    };

    // Read the graphics (compressed or uncompressed)
    Ok(RomGraphic::read_from(rom, offset, length)?)
}

/// Reads the tileset palettes (they are 16 in ROM even though less are used for each tileset).
fn read_palettes(rom: &RomData, header: &TilesetHeader) -> MapTilesetResult<Vec<RomPalette>> {
    // Get the palettes offset
    let offset = header
        .palettes
        .offset()
        .ok_or_else(|| MapTilesetError::NoPalettes)?;

    let mut palettes = Vec::with_capacity(16);

    for index in 0..16 {
        let pal_offset = offset + index * RomPalette::get_size(rom);
        let pal: RomPalette = rom.read(pal_offset)?;
        palettes.push(pal);
    }

    Ok(palettes)
}

/// Reads the metatiles that make up the tileset.
fn read_metatiles(
    rom: &RomData,
    header: &TilesetHeader,
    num_metatiles: u16,
) -> MapTilesetResult<Vec<MetaTile>> {
    let offset = header
        .metatiles
        .offset()
        .ok_or_else(|| MapTilesetError::NoMetatiles)?;

    let num_metatiles = num_metatiles as usize;
    let mut metatiles = Vec::with_capacity(num_metatiles);

    for i in 0..num_metatiles {
        let metatile_offset = offset + i * 16;
        let metatile: MetaTile = rom.read(metatile_offset)?;
        metatiles.push(metatile);
    }

    Ok(metatiles)
}

/// Reads the attributes for each metatile.
fn read_attributes(
    rom: &RomData,
    header: &TilesetHeader,
    num_metatiles: u16,
) -> MapTilesetResult<Vec<MetatileAttributes>> {
    let offset = header
        .attributes
        .offset()
        .ok_or_else(|| MapTilesetError::NoAttributes)?;

    let num_metatiles = num_metatiles as usize;
    let mut attributes = Vec::with_capacity(num_metatiles);

    match rom.base {
        // Attributes are 4 bytes per metatile.
        RomBase::FireRed | RomBase::LeafGreen => {
            for i in 0..num_metatiles {
                let attr = rom.read_word(offset + i * 4)?;

                attributes.push(MetatileAttributes {
                    behavior: (attr & 0x000001ff) as u16,
                    terrain: (attr & 0x00003e00 >> 9) as u8,
                    encounter_type: ((attr & 0x07000000) >> 24) as u8,
                    layer_type: (((attr & 0x60000000) >> 29) as u8).into(),
                })
            }
        }

        // Attributes are 2 bytes per metatile.
        RomBase::Emerald | RomBase::Ruby | RomBase::Sapphire => {
            for i in 0..num_metatiles {
                let attr = rom.read_halfword(offset + i * 2)?;

                attributes.push(MetatileAttributes {
                    behavior: (attr & 0xff) as u16,
                    layer_type: (((attr & 0xf000) >> 12) as u8).into(),
                    terrain: 0,
                    encounter_type: 0,
                })
            }
        }
    }

    Ok(attributes)
}
