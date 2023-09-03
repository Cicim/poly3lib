use std::fmt::Debug;

use colored::Colorize;
use serde::{Deserialize, Serialize};
use serde_repr::*;

use rom_data::{
    types::{RomGraphic, RomPalette, RomReadableType, RomSizedType, RomTileMapEntry},
    Offset, RomBase, RomData, RomIoError,
};

use crate::Rom;

use super::{MapTilesetError, MapTilesetResult, TilesetAnimationList, TilesetHeader};

// ANCHOR Metatile
#[derive(Default, Clone, Copy, Serialize, Deserialize)]
pub struct MetaTile(pub [RomTileMapEntry; 4], pub [RomTileMapEntry; 4]);

impl Debug for MetaTile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[ [{:?} {:?} {:?} {:?}] [{:?} {:?} {:?} {:?}] ]",
            self.0[0], self.0[1], self.0[2], self.0[3], self.1[0], self.1[1], self.1[2], self.1[3]
        )
    }
}

impl RomSizedType for MetaTile {
    fn get_size(_: &RomData) -> usize {
        16
    }
    fn get_alignment(_: &RomData) -> usize {
        2
    }
}
impl RomReadableType for MetaTile {
    fn read_from(rom: &RomData, offset: Offset) -> Result<Self, RomIoError> {
        // Read all 8 maptiles
        let bot1: RomTileMapEntry = rom.read_halfword(offset)?.into();
        let bot2: RomTileMapEntry = rom.read_halfword(offset + 2)?.into();
        let bot3: RomTileMapEntry = rom.read_halfword(offset + 4)?.into();
        let bot4: RomTileMapEntry = rom.read_halfword(offset + 6)?.into();
        let top1: RomTileMapEntry = rom.read_halfword(offset + 8)?.into();
        let top2: RomTileMapEntry = rom.read_halfword(offset + 10)?.into();
        let top3: RomTileMapEntry = rom.read_halfword(offset + 12)?.into();
        let top4: RomTileMapEntry = rom.read_halfword(offset + 14)?.into();

        Ok(Self([bot1, bot2, bot3, bot4], [top1, top2, top3, top4]))
    }
}

// ANCHOR Behavior and background bytes
/// The attributes of a metatile organized into a struct
/// for interoperability between versions.
#[derive(Default, Serialize, Deserialize)]
pub struct MetatileAttributes {
    /// Specifies the behavior of the tile
    ///
    /// Present in all versions
    pub behavior: u16,
    /// Specifies the terrain type of the tile
    /// NORMAL, GRASS, WATER or WATERFALL
    ///
    /// Present only in FireRed
    pub terrain: u8,
    /// Specifies the encounter type of the tile
    /// NONE, LAND or WATER
    ///
    /// Present only in FireRed
    pub encounter_type: u8,
    /// Specifies whether the player passed behind the top layer
    ///
    /// Present in all versions
    pub layer_type: MetatileLayerType,
}

impl Debug for MetatileAttributes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let behavior = format!("B={:02X}", self.behavior).yellow().to_string();
        let layer_type = format!("L={}", self.layer_type as u8)
            .bright_cyan()
            .to_string();

        let mut strings = vec![behavior, layer_type];
        if self.terrain != 0 {
            strings.push(format!("T={}", self.terrain).bright_green().to_string())
        }
        if self.encounter_type != 0 {
            strings.push(
                format!("E={}", self.encounter_type)
                    .bright_green()
                    .to_string(),
            )
        }

        write!(f, "Attr({})", strings.join(", "))
    }
}

// TODO Find a better way to get these
impl RomSizedType for MetatileAttributes {
    fn get_size(rom: &RomData) -> usize {
        match rom.base {
            RomBase::Ruby | RomBase::Sapphire | RomBase::Emerald => 2,
            RomBase::FireRed | RomBase::LeafGreen => 4,
        }
    }
    fn get_alignment(rom: &RomData) -> usize {
        match rom.base {
            RomBase::Ruby | RomBase::Sapphire | RomBase::Emerald => 2,
            RomBase::FireRed | RomBase::LeafGreen => 4,
        }
    }
}

#[derive(Debug, Default, Serialize_repr, Deserialize_repr, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum MetatileLayerType {
    #[default]
    /// Metatile uses middle and top bg layers
    Normal = 0,
    /// Metatile uses bottom and middle bg layers
    Covered = 1,
    /// Metatile uses bottom and top bg layers
    Split = 2,

    /// Metatile uses bottom, middle and top bg layers
    /// and retrieves the top layer from the next metatile.
    ThreeLayers = 3,
}

impl Into<MetatileLayerType> for u8 {
    fn into(self) -> MetatileLayerType {
        match self % 4 {
            0 => MetatileLayerType::Normal,
            1 => MetatileLayerType::Covered,
            2 => MetatileLayerType::Split,
            3 => MetatileLayerType::ThreeLayers,
            _ => unreachable!("The layer type compared after a modulo 4 operation"),
        }
    }
}

// ANCHOR TilesetData struct
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
    /// Animation data for the tileset
    ///
    /// This is loaded at a later time explicitly
    pub animations: Option<TilesetAnimationList>,
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
            // Animations are loaded at a later time
            animations: None,
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
