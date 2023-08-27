use serde::{ser::SerializeStruct, Deserialize, Serialize, Serializer};

use crate::{
    lz77::{self, Lz77Header},
    Offset, RomData, RomIoError,
};

use super::RomTile;

/// Compressed or uncompressed collection of [`RomTile`]s.
pub enum RomGraphic {
    /// New [`RomGraphic`] to write to ROM.
    New(Vec<RomTile>),

    /// Compressed tile data read from ROM.
    Compressed {
        /// Offset to the LZ77 header.
        offset: Offset,
        /// Decompressed tiles.
        tiles: Vec<RomTile>,
    },

    /// Uncompressed tile data read from the ROM.
    Uncompressed {
        /// Offset to the start of the tiles
        offset: Offset,
        /// Number of tiles
        tiles: Vec<RomTile>,
        /// Tables to write
        read_len: usize,
    },
}

impl RomGraphic {
    /// Returns a new RomGraphic object starting from a collection of tiles.
    ///
    /// Equivalent to
    /// ```no_run
    /// RomGraphic::New(tiles)
    pub fn new(tiles: Vec<RomTile>) -> Self {
        Self::New(tiles)
    }

    // ANCHOR Reading
    /// Reads compressed graphics data if `length` is set to `None`, otherwise it
    /// reads uncompressed graphics data with the given number of tiles.
    pub fn read_from(
        rom: &RomData,
        offset: Offset,
        length: Option<usize>,
    ) -> Result<Self, RomIoError> {
        match length {
            Some(length) => Self::read_uncompressed(rom, offset, length),
            None => Self::read_compressed(rom, offset),
        }
    }

    /// Reads compressed graphics data from ROM.
    pub fn read_compressed(rom: &RomData, offset: Offset) -> Result<Self, RomIoError> {
        // Read some compressed data
        let decompressed = rom.read_compressed(offset)?;
        // Assert the correct number of tiles
        if decompressed.len() % 32 != 0 {
            Err(RomIoError::InvalidDecompressedSize(decompressed.len()))?
        }

        let mut tiles = Vec::new();

        // Read the tiles
        for tile_bytes in decompressed.chunks_exact(32) {
            // SAFETY: We asserted that the bytes are divisible by 32, so, every
            // chunk will certainly contain exactly 32 elements.
            let tile_bytes: &[u8; 32] = tile_bytes.try_into().unwrap();

            let tile = RomTile::read_packed(tile_bytes);
            tiles.push(tile);
        }

        Ok(Self::Compressed { offset, tiles })
    }

    /// Reads uncompressed graphics data with the given number of tiles from ROM.
    pub fn read_uncompressed(
        rom: &RomData,
        offset: Offset,
        length: usize,
    ) -> Result<Self, RomIoError> {
        // Read the slice that contains all the data
        let slice = rom.read_slice(offset, length * 32)?;

        let mut tiles = Vec::new();

        // Read the tiles
        for tile_bytes in slice.chunks_exact(32) {
            // SAFETY: We asserted that the bytes are divisible by 32, so, every
            // chunk will certainly contain exactly 32 elements.
            let tile_bytes: &[u8; 32] = tile_bytes.try_into().unwrap();

            let tile = RomTile::read_packed(tile_bytes);
            tiles.push(tile);
        }

        Ok(Self::Compressed { offset, tiles })
    }

    // ANCHOR Writing
    /// Writes the given graphics data to ROM in the specified format, if necessary
    /// overwriting the old data at its offset and with its format.
    ///
    /// Returns the new offset of the data.
    pub fn write(self, rom: &mut RomData, compressed: bool) -> Result<Offset, RomIoError> {
        // Get the byte representation of the new data
        let bytes = if compressed {
            lz77::compress(&self.to_bytes())
        } else {
            self.to_bytes()
        };

        // Get the new size to repoint (header bytes if compressed)
        let inflated_size = self.tiles().len() * 32;
        let new_size = bytes.len() + (compressed as usize) * 4;

        // Get the old size (if present)
        let old_offset_and_size = match self {
            // The data was not written to ROM.
            RomGraphic::New(_) => None,

            // The data had an offset, and a specified size
            RomGraphic::Uncompressed {
                offset, read_len, ..
            } => Some((offset, read_len * 32)),

            // The data was compressed in ROM, we have to get its size
            RomGraphic::Compressed { offset, tiles: _ } => {
                match lz77::get_deflated_size(rom, offset) {
                    Ok(deflated_size) => Some((offset, deflated_size + 4)),
                    Err(_) => None,
                }
            }
        };

        let new_offset = match old_offset_and_size {
            // If there is no old stuff, find new space
            None => rom.find_free_space(new_size, 4)?,
            // If you have old stuff, try to repoint
            Some((old_offset, old_size)) => rom.repoint_offset(old_offset, old_size, new_size)?,
        };

        // Write the data to the new spot
        if compressed {
            // Write the header
            rom.write(new_offset, Lz77Header::new(inflated_size))?;
            // Write the compressed data
            rom.write_slice(new_offset + 4, &bytes)?;
        } else {
            rom.write_slice(new_offset, &bytes)?;
        }

        Ok(new_offset)
    }

    /// Returns the inner tiles.
    pub fn tiles(&self) -> &Vec<RomTile> {
        match self {
            RomGraphic::New(tiles) => tiles,
            RomGraphic::Compressed { tiles, .. } => tiles,
            RomGraphic::Uncompressed { tiles, .. } => tiles,
        }
    }

    /// Converts the inner tiles to their byte representation.
    fn to_bytes(&self) -> Vec<u8> {
        let tiles = self.tiles();
        let mut bytes = Vec::new();

        for tile in tiles {
            let mut buffer = [0; 32];
            tile.write_packed(&mut buffer);
            bytes.extend(buffer);
        }

        bytes
    }
}

// ANCHOR Serde impl
impl Serialize for RomGraphic {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use RomGraphic::*;

        match self {
            // New is serialized to { tiles: ... }
            New(tiles) => {
                let mut state: _ = serializer.serialize_struct("RomGraphic", 1)?;
                state.serialize_field("tiles", tiles)?;
                state.end()
            }

            // Compressed is serialized to { offset: ..., tiles: ... }
            Compressed { offset, tiles } => {
                let mut state: _ = serializer.serialize_struct("RomGraphic", 2)?;
                state.serialize_field("tiles", tiles)?;
                state.serialize_field("offset", offset)?;
                state.end()
            }

            // Uncompressed is serialized to { offset: ..., tiles: ..., read_len: ... }
            Uncompressed {
                offset,
                tiles,
                read_len,
            } => {
                let mut state: _ = serializer.serialize_struct("RomGraphic", 3)?;
                state.serialize_field("tiles", tiles)?;
                state.serialize_field("offset", offset)?;
                state.serialize_field("read_len", read_len)?;
                state.end()
            }
        }
    }
}

impl<'de> Deserialize<'de> for RomGraphic {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "lowercase")]
        enum Field {
            Tiles,
            Offset,
            ReadLen,
        }

        // Visitor for RomGraphic
        struct RomGraphicVisitor;

        impl<'de> serde::de::Visitor<'de> for RomGraphicVisitor {
            type Value = RomGraphic;

            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.write_str("valid JSON representation of RomGraphic")
            }

            fn visit_map<A: serde::de::MapAccess<'de>>(
                self,
                mut map: A,
            ) -> Result<Self::Value, A::Error> {
                let mut offset: Option<usize> = None;
                let mut read_len: Option<usize> = None;
                let mut tiles: Option<Vec<RomTile>> = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Tiles => {
                            if tiles.is_some() {
                                return Err(serde::de::Error::duplicate_field("tiles"));
                            }
                            tiles = Some(map.next_value()?);
                        }
                        Field::Offset => {
                            if offset.is_some() {
                                return Err(serde::de::Error::duplicate_field("offset"));
                            }
                            offset = Some(map.next_value()?);
                        }
                        Field::ReadLen => {
                            if read_len.is_some() {
                                return Err(serde::de::Error::duplicate_field("read_len"));
                            }
                            read_len = Some(map.next_value()?);
                        }
                    }
                }

                match (tiles, offset, read_len) {
                    // New
                    (Some(tiles), None, None) => Ok(RomGraphic::New(tiles)),
                    (Some(tiles), Some(offset), None) => {
                        Ok(RomGraphic::Compressed { offset, tiles })
                    }
                    (Some(tiles), Some(offset), Some(read_len)) => Ok(RomGraphic::Uncompressed {
                        offset,
                        tiles,
                        read_len,
                    }),

                    (None, _, _) => Err(serde::de::Error::missing_field("tiles")),
                    (Some(_), None, Some(_)) => Err(serde::de::Error::missing_field("offset")),
                }
            }
        }

        deserializer.deserialize_any(RomGraphicVisitor)
    }
}

// ANCHOR Tests
#[cfg(test)]
mod tests {
    use super::*;

    fn get_test_rom() -> RomData {
        let mut rom = RomData::new(crate::RomBase::FireRed, 0x1000);
        let num_tiles = 16;

        let tile_bytes = [0xA5].repeat(num_tiles * 32);

        // Write them uncompressed at 0x0
        rom.write_slice(0x0, &tile_bytes).unwrap();
        // Write them compressed at 0x200
        rom.write_compressed(0x200, &tile_bytes).unwrap();

        rom
    }

    #[test]
    fn test_tiles() {
        let tiles = vec![RomTile::default(); 16];
        let graphic = RomGraphic::new(tiles.clone());

        assert_eq!(graphic.tiles(), &tiles);

        let graphic2 = RomGraphic::Compressed {
            offset: 0,
            tiles: tiles.clone(),
        };
        assert_eq!(graphic2.tiles(), &tiles);

        let graphic3 = RomGraphic::Uncompressed {
            offset: 0,
            tiles: tiles.clone(),
            read_len: 16,
        };
        assert_eq!(graphic3.tiles(), &tiles);
    }

    #[test]
    fn test_read_compressed() {
        let rom = get_test_rom();
        let graphic = RomGraphic::read_compressed(&rom, 0x200).unwrap();

        assert_eq!(graphic.tiles().len(), 16);
    }

    #[test]
    fn test_read_uncompressed() {
        let rom = get_test_rom();
        let graphic = RomGraphic::read_uncompressed(&rom, 0x0, 16).unwrap();

        assert_eq!(graphic.tiles().len(), 16);
    }

    #[test]
    fn test_write_compressed() {
        let mut rom = get_test_rom();
        let graphic = RomGraphic::read_compressed(&rom, 0x200).unwrap();
        let read_tiles = graphic.tiles().clone();

        let new_offset = graphic.write(&mut rom, true).unwrap();
        let new_graphic = RomGraphic::read_compressed(&rom, new_offset).unwrap();

        assert_eq!(&read_tiles, new_graphic.tiles());
    }

    #[test]
    fn test_write_uncompressed() {
        let mut rom = get_test_rom();
        let graphic = RomGraphic::read_uncompressed(&rom, 0x0, 16).unwrap();
        let read_tiles = graphic.tiles().clone();

        let new_offset = graphic.write(&mut rom, false).unwrap();
        let new_graphic = RomGraphic::read_uncompressed(&rom, new_offset, 16).unwrap();

        assert_eq!(&read_tiles, new_graphic.tiles());
    }

    #[test]
    fn test_write_new_uncompressed() {
        let mut rom = get_test_rom();
        let graphic = RomGraphic::new(vec![RomTile::default(); 16]);
        let read_tiles = graphic.tiles().clone();

        let new_offset = graphic.write(&mut rom, false).unwrap();
        let new_graphic = RomGraphic::read_uncompressed(&rom, new_offset, 16).unwrap();

        assert_eq!(&read_tiles, new_graphic.tiles());
    }

    #[test]
    fn test_write_new_compressed() {
        let mut rom = get_test_rom();
        let graphic = RomGraphic::new(vec![RomTile::default(); 16]);
        let read_tiles = graphic.tiles().clone();

        let new_offset = graphic.write(&mut rom, true).unwrap();
        let new_graphic = RomGraphic::read_compressed(&rom, new_offset).unwrap();

        assert_eq!(&read_tiles, new_graphic.tiles());
    }

    #[test]
    fn test_serde() {
        let rom = get_test_rom();
        let graphic = RomGraphic::read_compressed(&rom, 0x200).unwrap();

        let json = serde_json::to_string(&graphic).unwrap();
        let new_graphic: RomGraphic = serde_json::from_str(&json).unwrap();

        assert_eq!(graphic.tiles(), new_graphic.tiles());
    }
}
