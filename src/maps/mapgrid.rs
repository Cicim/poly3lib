use crate::{
    rom::{Rom, RomType},
    values::RomValueError,
};

#[derive(Debug)]
/// Values to extract all parts of a map grid entry
pub struct MapGridMasks {
    /// Same as `MAPGRID_UNDEFINED`
    pub metatile_id_mask: u16,
    pub collision_shift: u8,
    pub collision_mask: u16,
    pub elevation_shift: u8,
    pub elevation_mask: u16,
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
    /// There are only two currently possible configurations.
    ///
    /// The one in which there are two collision bits (default on every ROM, but inefficient)
    /// and the one in which there is one collision bit.
    ///
    /// Right now it only makes sense to move to the one collision bit configuration.
    ///
    /// Having less level bits could be supported in the future, but requires more
    /// work for very little gain.
    pub fn set_one_collision_bit(rom: &mut Rom) -> Result<(), RomValueError> {
        let rom_type = rom.rom_type;

        // Compute every value to set
        let mapgrid_metatile_id_mask = 0x07FF;
        let mapgrid_collision_mask = 0x0800;
        let mapgrid_elevation_mask: u16 = 0xF000;
        let mapgrid_collision_shift: u8 = 11;
        let mapgrid_elevation_shift: u8 = 12;

        let neg_mapgrid_elevation_mask = !mapgrid_elevation_mask;
        let temp = mapgrid_metatile_id_mask as u32;
        let cpu_fill_value = (temp << 16) | temp;

        // Write the values
        // Metatile id mask
        rom.write(get_cpu_fill_value_ref(rom_type), cpu_fill_value)?;
        rom.write_same(
            &get_metatile_id_mask_refs(rom_type),
            Rom::write_word_value,
            mapgrid_metatile_id_mask,
        )?;

        // Collision mask and shift
        rom.write_same(
            &get_collision_mask_refs(rom_type),
            Rom::write_mov_lsl_value,
            mapgrid_collision_mask,
        )?;
        rom.write_lsr_shift(get_collision_shift_ref(rom_type), mapgrid_collision_shift)?;

        // Elevation mask and shift
        rom.write_word_value(
            get_neg_elevation_mask_ref(rom_type),
            neg_mapgrid_elevation_mask as u32,
        )?;
        rom.write_mov_lsl_value(
            get_elevation_mask_ref(rom_type),
            mapgrid_elevation_mask as u32,
        )?;

        rom.write_lsr_shift(get_elevation_shift_ref(rom_type), mapgrid_elevation_shift)?;

        Ok(())
    }

    /// Reads the map grid sizes from the ROM.
    ///
    /// Only returns true if everything is correct
    pub fn read(rom: &Rom) -> Result<Self, RomValueError> {
        let rom_type = rom.rom_type;

        // Metatile id mask
        let fill_value: u32 = rom.read(get_cpu_fill_value_ref(rom_type))?;
        let metatile_id_mask: u32 = rom.read_and_assert_all_equal(
            &get_metatile_id_mask_refs(rom_type),
            Rom::read_word_value,
        )?;

        // Make sure the two values are the same
        if (metatile_id_mask << 16) | metatile_id_mask != fill_value {
            return Err(RomValueError::Mismatch);
        }

        // Collision mask and shift
        let collision_mask = rom.read_and_assert_all_equal(
            &get_collision_mask_refs(rom_type),
            Rom::read_mov_lsl_value,
        )? as u16;

        let collision_shift = rom.read_lsr_shift(get_collision_shift_ref(rom_type))?;

        // Elevation mask and shift
        let elevation_mask_neg = rom.read_word_value(get_neg_elevation_mask_ref(rom_type))? as u16;
        let elevation_mask = rom.read_mov_lsl_value(get_elevation_mask_ref(rom_type))? as u16;

        if !elevation_mask_neg != elevation_mask {
            return Err(RomValueError::Mismatch);
        }
        let elevation_shift = rom.read_lsr_shift(get_elevation_shift_ref(rom_type))?;

        Ok(MapGridMasks {
            metatile_id_mask: metatile_id_mask as u16,
            collision_mask,
            collision_shift,
            elevation_shift,
            elevation_mask,
        })
    }
}

fn get_cpu_fill_value_ref(rom_type: RomType) -> usize {
    match rom_type {
        RomType::FireRed => 0x058a58,
        RomType::LeafGreen => 0x058a58,
        RomType::Ruby => 0x056044,
        RomType::Sapphire => 0x056048,
        RomType::Emerald => 0x087e60,
    }
}
fn get_collision_shift_ref(rom_type: RomType) -> usize {
    match rom_type {
        RomType::FireRed => 0x058e34,
        RomType::LeafGreen => 0x058e34,
        RomType::Ruby => 0x0563ee,
        RomType::Sapphire => 0x0563f2,
        RomType::Emerald => 0x08820e,
    }
}
fn get_elevation_shift_ref(rom_type: RomType) -> usize {
    match rom_type {
        RomType::FireRed => 0x058dae,
        RomType::LeafGreen => 0x058dae,
        RomType::Ruby => 0x05637c,
        RomType::Sapphire => 0x056380,
        RomType::Emerald => 0x08819c,
    }
}
fn get_elevation_mask_ref(rom_type: RomType) -> usize {
    match rom_type {
        RomType::FireRed => 0x058fce,
        RomType::LeafGreen => 0x058fce,
        RomType::Ruby => 0x0564fa,
        RomType::Sapphire => 0x0564fe,
        RomType::Emerald => 0x08831a,
    }
}
fn get_neg_elevation_mask_ref(rom_type: RomType) -> usize {
    match rom_type {
        RomType::FireRed => 0x058fe8,
        RomType::LeafGreen => 0x058fe8,
        RomType::Ruby => 0x056514,
        RomType::Sapphire => 0x056518,
        RomType::Emerald => 0x088334,
    }
}
fn get_metatile_id_mask_refs(rom_type: RomType) -> Vec<usize> {
    match rom_type {
        RomType::FireRed => [0x058e3c, 0x058db8, 0x058ecc, 0x059370],
        RomType::LeafGreen => [0x058e3c, 0x058db8, 0x058ecc, 0x059370],
        RomType::Ruby => [0x0563f8, 0x056384, 0x056468, 0x0567f8],
        RomType::Sapphire => [0x0563fc, 0x056388, 0x05646c, 0x0567fc],
        RomType::Emerald => [0x088218, 0x0881a4, 0x088288, 0x0886d4],
    }
    .to_vec()
}
fn get_collision_mask_refs(rom_type: RomType) -> Vec<usize> {
    match rom_type {
        RomType::FireRed => [0x058e2e, 0x06c5b8, 0x058e20, 0x058da0, 0x058eb0, 0x058f00],
        RomType::LeafGreen => [0x058e2e, 0x06c5b8, 0x058e20, 0x058da0, 0x058eb0, 0x058f00],
        RomType::Ruby => [0x0563e8, 0x067c74, 0x0563da, 0x05636e, 0x05644e, 0x056486],
        RomType::Sapphire => [0x0563ec, 0x067c78, 0x0563de, 0x056372, 0x056452, 0x05648a],
        RomType::Emerald => [0x088208, 0x09b954, 0x0881fa, 0x08818e, 0x08826e, 0x0882a6],
    }
    .to_vec()
}
