use serde::{Deserialize, Serialize};
use thiserror::Error;

use rom_data::{
    rom_struct,
    types::{RomPointer, RomSizedType, RomText},
    RomBase, RomIoError,
};

use crate::{Rom, RomTable};

rom_struct!(RegionMapLocation {
    #[for(base(Ruby, Sapphire, Emerald), type(u8))]
    u16 x, y, width, height;

    struct RomText *name;
} readonly);

/// Struct for dumping all the region map locations.
#[derive(Debug, Serialize, Deserialize)]
pub struct MapNamesDump {
    /// Name of each map location.
    pub names: Vec<RomText>,

    /// First index of the region map locations in the map sections table
    /// (since Kanto starts at `0x58` in FrLg).
    pub start_index: u8,
}

#[derive(Debug, Error)]
pub enum RegionMapError {
    #[error("The region map location table was not loaded")]
    TableNotLoaded,

    #[error("IO Error: {0}")]
    IoError(#[from] RomIoError),
}

type RegionMapResult<T = ()> = Result<T, RegionMapError>;

impl Rom {
    /// Dumps all the names of the region map sections.
    pub fn read_region_map_sections_names(&self) -> RegionMapResult<MapNamesDump> {
        match get_table(self)? {
            RegionMapLocationTables::FrLg {
                section_start,
                names,
                ..
            } => {
                let names = names.read_elements(
                    self,
                    |rom, offset| {
                        let offset = rom.read_offset(offset)?;
                        let name = rom.read::<RomText>(offset)?;
                        Ok(name)
                    },
                    4,
                )?;
                Ok(MapNamesDump {
                    names,
                    start_index: *section_start,
                })
            }

            RegionMapLocationTables::RSE(table) => {
                let names = table.read_elements(
                    self,
                    |rom, offset| {
                        let location = rom.read::<RegionMapLocation>(offset)?;
                        match location.name {
                            RomPointer::Valid(_, name) => Ok(name),
                            _ => Ok(RomText::new_empty()),
                        }
                    },
                    RegionMapLocation::get_size(&self.data),
                )?;

                Ok(MapNamesDump {
                    names,
                    start_index: 0,
                })
            }
        }
    }
}

fn get_table(rom: &Rom) -> RegionMapResult<&RegionMapLocationTables> {
    rom.refs
        .region_map_locations
        .as_ref()
        .ok_or(RegionMapError::TableNotLoaded)
}

// These cannot be used as mapsecs
// METLOC_SPECIAL_EGG = 0xFD
// METLOC_IN_GAME_TRADE = 0xFE
// METLOC_FATEFUL_ENCOUNTER = 0xFF

#[derive(Clone, Serialize, Deserialize)]
pub enum RegionMapLocationTables {
    FrLg {
        section_start: u8,
        names: RomTable,
        topleft_corners: RomTable,
        dimensions: RomTable,
    },
    RSE(RomTable),
}

pub fn init_table(rom: &mut Rom) -> Result<(), RomIoError> {
    if rom.refs.region_map_locations.is_some() {
        return Ok(());
    }

    let table = match rom.base() {
        RomBase::Ruby | RomBase::Sapphire | RomBase::Emerald => read_rse_table(rom)?,
        RomBase::FireRed | RomBase::LeafGreen => read_frlg_table(rom)?,
    };

    rom.refs.region_map_locations = Some(table);

    Ok(())
}

/// Reads the region map locations tables (which in FrLg are three separate tables)
fn read_frlg_table(rom: &mut Rom) -> Result<RegionMapLocationTables, RomIoError> {
    // fire_red
    // > names: 0xc0c94, 0xc4db8
    // > topleft_corners: 0xc3d3c
    // > dimensions: 0xc3d38
    // leaf_green
    // > names: 0xc0c68, 0xc4d8c
    // > topleft_corners: 0xc3d10
    // > dimensions: 0xc3d0c
    let (name_ref, topleft_ref, dim_ref) = match rom.base() {
        RomBase::FireRed => (0xc0c94, 0xc3d3c, 0xc3d38),
        RomBase::LeafGreen => (0xc0c68, 0xc3d10, 0xc3d0c),
        _ => unreachable!("method for frlg"),
    };

    let names_offset = rom.data.read_offset(name_ref)?;
    let topleft_offset = rom.data.read_offset(topleft_ref)?;
    let dim_offset = rom.data.read_offset(dim_ref)?;

    // Read the names table (which you can verify)
    let names = RomTable::extract_from(
        names_offset,
        &rom.data,
        |rom, offset| match rom.read::<RomPointer<RomText>>(offset) {
            Ok(RomPointer::Valid(_, name)) => Ok(name.byte_size() < 0x40),
            _ => Ok(false),
        },
        4,
    )?;

    // Read the topleft corners and dimensions tables (with the same length as names)
    let topleft_corners = RomTable::new(rom, topleft_offset, names.length);
    let dimensions = RomTable::new(rom, dim_offset, names.length);

    // Read the MAPSEC_KANTO value
    // TODO Where is this value?
    let section_start = 0x58;

    Ok(RegionMapLocationTables::FrLg {
        section_start,
        names,
        topleft_corners,
        dimensions,
    })
}

/// Reads the region map locations table (which in RSE is just one table)
fn read_rse_table(rom: &mut Rom) -> Result<RegionMapLocationTables, RomIoError> {
    // Here is a list of valid references to get this table.
    // emerald
    //     0x123b44, 0x123d54, 0x12459c, 0x124654, 0x13d814
    // ruby
    //     0xfb550, 0xfb5fc, 0xfb754, 0xfbfe0, 0xfc070, 0x11172c
    // sapphire
    //     0xfb550, 0xfb5fc, 0xfb754, 0xfbfe0, 0xfc070, 0x11172c
    // REVIEW Check if you can use a better reference. For now we take the first one.
    let reference = match rom.base() {
        RomBase::Ruby => 0xfb550,
        RomBase::Sapphire => 0xfb550,
        RomBase::Emerald => 0x123b44,
        RomBase::FireRed | RomBase::LeafGreen => unreachable!("method for rse"),
    };

    // Get the offset at that reference
    let table_offset = rom.data.read_offset(reference)?;

    let table = RomTable::extract_from(
        table_offset,
        &rom.data,
        |rom, offset| match rom.read::<RegionMapLocation>(offset) {
            Ok(loc) => Ok(loc.name.is_valid() && loc.width < 0x20 && loc.height < 0x20),
            Err(_) => Ok(false),
        },
        RegionMapLocation::get_size(&rom.data),
    )?;

    Ok(RegionMapLocationTables::RSE(table))
}
