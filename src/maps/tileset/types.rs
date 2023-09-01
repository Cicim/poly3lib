use std::fmt::Debug;

use colored::Colorize;
use rom_data::{
    types::{RomReadableType, RomSizedType, RomTileMapEntry},
    Offset, RomData, RomIoError,
};
use serde::{Deserialize, Serialize};
use serde_repr::*;

// ANCHOR Metatile
#[derive(Default, Clone, Copy, Serialize, Deserialize)]
pub struct MetaTile {
    pub bot: [RomTileMapEntry; 4],
    pub top: [RomTileMapEntry; 4],
}

impl Debug for MetaTile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[ [{:?} {:?} {:?} {:?}] [{:?} {:?} {:?} {:?}] ]",
            self.bot[0],
            self.bot[1],
            self.bot[2],
            self.bot[3],
            self.top[0],
            self.top[1],
            self.top[2],
            self.top[3]
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

        Ok(Self {
            bot: [bot1, bot2, bot3, bot4],
            top: [top1, top2, top3, top4],
        })
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
