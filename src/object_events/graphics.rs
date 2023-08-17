use gba_macro::gba_struct;
use gba_types::{
    colors::GBAPalette,
    pointers::{Nothing, PointedData},
};
use serde::Serialize;

use crate::{
    graphics::Graphic,
    object_events::subsprites::OamDataForObjectEvents,
    refs::{TableInitError, TablePointer},
    rom::{Rom, RomType},
};

use super::{
    anims::ObjectEventAnim,
    subsprites::{get_shape, SubspriteTables},
    ObjectEventError,
};

gba_struct!(ObjectEventGraphicsInfo {
    u16 tile_tag;
    u16 palette_tag;
    u16 reflection_palette_tag;
    u16 size;
    i16 width;
    i16 height;
    u8 palette_slot:4;
    u8 shadow_size:2;
    u8 inanimate:1;
    u8 disable_reflection_palette_load:1;
    u8 tracks;
    // const struct OamData *oam;
    // const struct SubspriteTable *subspriteTables;
    // const union AnimCmd *const *anims;
    // const struct SpriteFrameImage *images;
    // const union AffineAnimCmd *const *affineAnims;
    void* oam;
    void* subsprite_tables;
    void* anims;
    void* images;
    void* affine_anims;
});

/// Table of Object Event Graphics
pub struct ObjectEventGraphicsTable<'rom> {
    pub rom: &'rom mut Rom,
}

#[derive(Debug, Serialize)]
pub struct ObjectEventData {
    pub header: ObjectEventGraphicsInfo,

    pub width: u32,
    pub height: u32,

    /// Subsprites table for this object event.
    pub subsprite_tables: SubspriteTables,
    /// Palette for this object event.
    pub palette: GBAPalette,
    /// Graphics in the spritesheet. Overread for most objects,
    /// since we don't know how many graphics there are.
    pub images: Vec<Graphic>,
    /// Animations for this object event. Overread for most objects,
    /// since we don't know how many animations there are.
    pub anims: Vec<ObjectEventAnim>,
}

impl<'rom> ObjectEventGraphicsTable<'rom> {
    /// Initialize the table
    pub fn init(rom: &'rom mut Rom) -> Result<Self, TableInitError> {
        if rom.refs.object_event_gfx_table.is_none() || rom.refs.object_event_pal_table.is_none() {
            let gfx = init_object_events_table(rom)?;
            rom.refs.object_event_gfx_table = Some(gfx);

            let pal = init_object_event_sprite_palettes(rom)?;
            rom.refs.object_event_pal_table = Some(pal);
        }

        Ok(Self { rom })
    }

    /// Reads all the data necessary to render this object event and all its animations.
    pub fn read_data(&self, index: usize) -> Result<ObjectEventData, ObjectEventError> {
        let header = self.read_header(index)?;

        // Read the subsprites table
        let subsprites_offset = header
            .subsprite_tables
            .offset()
            .ok_or(ObjectEventError::InvalidSubspritesTable)?;
        let subsprite_tables = SubspriteTables::read(self.rom, subsprites_offset)?;

        // Read the palette
        let palette = self.get_palette_by_tag(header.palette_tag)?;

        // Read the images
        let images_offset = header
            .images
            .offset()
            .ok_or(ObjectEventError::InvalidImages)?;
        let images = self.read_graphics_list(images_offset)?;

        // Read the animations
        let anims_offset = header
            .anims
            .offset()
            .ok_or(ObjectEventError::InvalidAnimations)?;
        let anims = self.read_animations_list(anims_offset)?;

        // Read the OamData
        let oam_data_offset = header
            .oam
            .offset()
            .ok_or(ObjectEventError::InvalidOamData)?;
        let oam: OamDataForObjectEvents = self.rom.read(oam_data_offset)?;
        let size = get_shape(oam.size as u16, oam.shape as u16);

        Ok(ObjectEventData {
            header,
            subsprite_tables,
            palette,
            images,
            anims,
            width: size.0 as u32,
            height: size.1 as u32,
        })
    }

    /// Gets a [`ObjectEventGraphicsInfo`] from the table.
    pub fn read_header(&self, index: usize) -> Result<ObjectEventGraphicsInfo, ObjectEventError> {
        let table = self.get_gfx_table()?;

        if index > table.size {
            return Err(ObjectEventError::InvalidGraphicsId(index as u8));
        }

        let offset = table.offset + index * 4;
        let struct_offset = self.rom.read_ptr(offset)?;

        Ok(self.rom.read(struct_offset)?)
    }

    /// Returns a [`GBAPalette`] from a palette tag.
    pub fn get_palette_by_tag(&self, tag: u16) -> Result<GBAPalette, ObjectEventError> {
        let table = self.get_pal_table()?;

        // Find the offset for the given tag
        for i in 0..table.size {
            let pal_offset = table.offset + i * 8 + 4;
            let curr_tag: u16 = self.rom.read(pal_offset)?;

            if curr_tag == tag {
                let offset = table.offset + i * 8;
                let pointer = self.rom.read_ptr(offset)?;
                let palette: GBAPalette = self.rom.read(pointer)?;
                return Ok(palette);
            }
        }

        Err(ObjectEventError::PaletteNotFound(tag))
    }

    /// Returns a list of graphics given the offset that points to the table of
    /// `SpriteFrameImage` structs, defined as follows:
    ///
    /// ```c
    /// struct SpriteFrameImage
    /// {
    ///     const void *data;
    ///     u16 size;
    /// };
    /// ```
    ///
    /// Where `data` is a pointer to the actual graphics data, and `size` is the
    /// number of bytes in the graphics data (divide by 32 to get the number of tiles).
    fn read_graphics_list(&self, offset: usize) -> Result<Vec<Graphic>, ObjectEventError> {
        let mut list = vec![];

        let mut curr_offset = offset;
        // The biggest one has size 20 in FireRed. We're definitely overreading for everything else
        // but we can't find a heuristic for knowing when to stop.
        for _ in 0..20 {
            let size = self.rom.read::<u16>(curr_offset + 4)? / 32;
            let offset = self.rom.read::<PointedData<Nothing>>(curr_offset)?;

            // If the offset is not writable, we've reached the end of the list
            if !offset.is_writable() {
                break;
            }
            let gfx = Graphic::read(self.rom, offset.offset_unchecked(), Some(size as usize))?;
            list.push(gfx);

            curr_offset += 8;
        }

        Ok(list)
    }

    /// Returns a list of animations at the given offset.
    fn read_animations_list(
        &self,
        offset: usize,
    ) -> Result<Vec<ObjectEventAnim>, ObjectEventError> {
        let mut list = vec![];

        let mut curr_offset = offset;
        for _ in 0..40 {
            let offset = self.rom.read::<PointedData<Nothing>>(curr_offset)?;
            if !offset.is_writable() {
                break;
            }

            let anim = ObjectEventAnim::read(self.rom, offset.offset_unchecked())?;
            list.push(anim);

            curr_offset += 4;
        }

        Ok(list)
    }

    /// Returns the [`TablePointer`] for the graphics table.
    fn get_gfx_table(&self) -> Result<&TablePointer, ObjectEventError> {
        if let Some(table) = &self.rom.refs.object_event_gfx_table {
            Ok(table)
        } else {
            Err(ObjectEventError::TableNotInitialized)
        }
    }

    /// Returns the [`TablePointer`] for the palette table.
    fn get_pal_table(&self) -> Result<&TablePointer, ObjectEventError> {
        if let Some(table) = &self.rom.refs.object_event_pal_table {
            Ok(table)
        } else {
            Err(ObjectEventError::TableNotInitialized)
        }
    }
}

impl Rom {
    /// Returns the [`ObjectEventGraphicsTable`] struct for this ROM.
    ///
    /// # Panics
    /// If the object event graphics table has not been initialized.
    pub fn objevent_graphics(&mut self) -> ObjectEventGraphicsTable {
        ObjectEventGraphicsTable::init(self).unwrap_or_else(|_| {
            panic!("You have to initialize the object event graphics before calling rom.objevent_graphics()!")
        })
    }
}

fn init_object_events_table(rom: &Rom) -> Result<TablePointer, TableInitError> {
    // Read the stuff from the correct offsets
    let (ptr_ref, size_ref) = match rom.rom_type {
        RomType::Sapphire => (0x5bc40, 0x5bc2c),
        RomType::Ruby => (0x5bc3c, 0x5bc28),
        RomType::LeafGreen | RomType::FireRed => (0x5f2f4, 0x5f2e0),
        RomType::Emerald => (0x8e6d8, 0x8e6c4),
    };

    // Read the pointer to the table
    let offset = rom
        .read_ptr(ptr_ref)
        .map_err(|_| TableInitError::InvalidTablePointer)?;

    // Read the size of the table
    let size = rom.read_byte(size_ref) as usize;

    // Return the table
    Ok(TablePointer {
        offset,
        size,
        references: rom.find_references(offset),
    })
}

fn init_object_event_sprite_palettes(rom: &Rom) -> Result<TablePointer, TableInitError> {
    let reference = match rom.rom_type {
        RomType::Ruby => 0x5beb4,
        RomType::Sapphire => 0x5beb8,
        RomType::LeafGreen | RomType::FireRed => 0x5f570,
        RomType::Emerald => 0x8e950,
    };

    // Get the offset
    let offset = rom
        .read_ptr(reference)
        .map_err(|_| TableInitError::InvalidTablePointer)?;

    // This table is composed of SpritePalette structs (data (ptr) + tag (u16))
    let mut size = 0;

    while size < 256 {
        let word = rom
            .read::<u32>(offset + size * 8)
            .map_err(|_| TableInitError::TableGoesOutOfBounds)?;

        if word == 0 {
            break;
        }

        size += 1;
    }

    Ok(TablePointer {
        offset,
        size,
        references: rom.find_references(offset),
    })
}
