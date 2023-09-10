use std::{collections::HashMap, fmt::Display};

use colored::Colorize;
use serde::{Deserialize, Serialize};

use rom_data::{Offset, RomBase, RomData, RomFileError, RomIoError};

// ANCHOR Rom struct
/// The main ROM object.
///
/// Every access to its data can be done via its `data` property.
#[derive(Clone)]
pub struct Rom {
    /// The [`RomData`] object that contains the ROM data.
    pub data: RomData,
    /// The references to the various tables in the ROM.
    pub refs: RomReferences,
}

impl Rom {
    /// Save the [`RomData`] and [`RomReferences`] to the given path.
    pub fn save(&self, path: &str) -> Result<(), Box<dyn std::error::Error>> {
        // Write the ROM data
        self.data.save(path)?;

        // Write the references
        self.save_refs(path)?;

        Ok(())
    }

    /// Saves the references to the given (after replacing .gba with .json).
    pub fn save_refs(&self, path: &str) -> Result<(), Box<dyn std::error::Error>> {
        let refs_path = std::path::Path::new(path).with_extension("json");
        let mut references = serde_json::to_string_pretty(&self.refs)?;
        references.push('\n');
        std::fs::write(refs_path, references)?;
        Ok(())
    }

    /// Loads the ROM data and references from the given path.
    ///
    /// If path is `/path/to/file/rom.gba`, it loads the ROM binary from
    /// `/path/to/file/rom.gba` and the references from `/path/to/file/rom.json`.
    pub fn load(path: &str) -> Result<Self, RomFileError> {
        // Load the ROM data
        let data = RomData::load(path)?;
        // Load the references
        let refs = Rom::load_references(path).unwrap_or_default();

        Ok(Self { data, refs })
    }
    /// Internal function to load the references
    fn load_references(path: &str) -> Option<RomReferences> {
        let refs_path = std::path::Path::new(path).with_extension("json");
        match std::fs::read_to_string(refs_path) {
            Ok(references) => serde_json::from_str(&references).ok(),
            Err(_) => None,
        }
    }

    // ANCHOR Rom helpers
    /// Access the `base` property of the data without having to write `self.data.base`.
    pub fn base(&self) -> RomBase {
        self.data.base
    }
}

impl Display for Rom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.data)?;
        writeln!(f, "{}", self.refs)
    }
}

// ANCHOR Rom references
use crate::maps::{
    scripts::ScriptCommandsTable, sections::RegionMapLocationTables, tileset::TilesetShortInfo,
};

#[derive(Default, Serialize, Deserialize, Clone)]
/// The references to the various tables in the ROM.
///
/// Also contains values that are not references to tables.
pub struct RomReferences {
    /// The table of all map groups in the ROM, as well as all map groups
    pub map_groups: Option<crate::maps::map::MapGroups>,
    /// The table of all layouts in the ROM.
    pub map_layouts: Option<RomTable>,
    /// The list of tilesets loaded from the ROM.
    pub map_tilesets: Option<HashMap<Offset, TilesetShortInfo>>,

    /// The list of script commands.
    pub script_cmds: Option<ScriptCommandsTable>,

    /// The region map locations (`mapsec`s)
    pub region_map_locations: Option<RegionMapLocationTables>,
}

impl Display for RomReferences {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "References:")?;

        macro_rules! write_field {
            ($name:ident) => {
                write!(f, "  {}: ", stringify!($name))?;
                match &self.$name {
                    Some(table) => writeln!(f, "{}", table)?,
                    None => writeln!(f, "{}", "Not loaded".italic())?,
                }
            };
        }

        // Printing map groups
        match self.map_groups {
            Some(ref groups) => {
                writeln!(f, "  map_groups: {}", groups.table)?;

                let mut index = 0;
                // Start printing all groups in groups of 8 per row
                for eight_groups in groups.groups.chunks(8) {
                    write!(f, "    ")?;
                    for group in eight_groups {
                        let index_str = format!("{:>3}", index).blue();
                        let length_str = format!("{:>3}", group.length).red();

                        write!(f, "{}: {}  ", index_str, length_str)?;
                        index += 1;
                    }
                    writeln!(f)?;
                }
            }
            None => {
                writeln!(f, "  map_groups: {}", "Not loaded".italic())?;
            }
        }

        // ANCHOR Fields of rom references
        write_field!(map_layouts);

        // Printing map tilesets
        match self.map_tilesets {
            Some(ref map) => {
                // Count the primary ones
                let primary_count = map.values().filter(|t| t.is_primary).count();
                let secondary_count = map.len() - primary_count;
                writeln!(
                    f,
                    "  map_tilesets: {} loaded ({} primary, {} secondary)",
                    map.len(),
                    primary_count,
                    secondary_count
                )?;
            }
            None => {
                writeln!(f, "  map_tilesets: {}", "Not loaded".italic())?;
            }
        }

        // Printing the scripts table
        match self.script_cmds {
            Some(ref scripts) => {
                // Only print the length, the rest is superfluous
                writeln!(
                    f,
                    "  script_cmds: {} (range {}-{})",
                    scripts.table,
                    "0x00".red(),
                    format!("0x{:02X}", scripts.len() - 1).red(),
                )?
            }
            None => writeln!(f, "  script_cmds: {}", "Not loaded".italic())?,
        }

        // Print the region map locations
        match &self.region_map_locations {
            None => writeln!(f, "  region_map_locations: {}", "Not loaded".italic())?,
            Some(RegionMapLocationTables::FrLg {
                section_start,
                names,
                ..
            }) => writeln!(
                f,
                "  region_map_locations: {} (start: {})",
                names,
                section_start.to_string().red()
            )?,
            Some(RegionMapLocationTables::RSE(table)) => {
                writeln!(f, "  region_map_locations: {}", table)?
            }
        }

        Ok(())
    }
}

// ANCHOR RomTable struct
#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct RomTable {
    /// The offset of the table in the ROM.
    pub offset: Offset,
    /// The length of this table (in elements, not bytes).
    pub length: usize,
    /// The references to this offset in the ROM.
    pub references: Vec<Offset>,
}

impl Display for RomTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.length.to_string().bright_red())?;
        write!(f, "@{}", format!("${:07X}", self.offset).green())?;
        if self.references.is_empty() {
            return Ok(());
        }

        write!(f, " <- ")?;
        let references = self
            .references
            .iter()
            .map(|offset| format!("${:07X}", offset).cyan().italic().to_string())
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "{}", references)
    }
}

impl RomTable {
    /// Creates a new RomTable given offset and size
    pub fn new(rom: &Rom, offset: Offset, length: usize) -> Self {
        let references = rom.data.find_references(offset, 4);

        Self {
            offset,
            length,
            references,
        }
    }

    /// Extracts a table's length and references given the start offset.
    ///
    /// The `is_valid` function is used to determine whether the element in
    /// the table is valid. The table ends when `is_valid` returns false.
    ///
    /// Requires the `table_offset`, NOT one of its references.
    pub fn extract_from<F>(
        table_offset: Offset,
        rom_data: &RomData,
        is_valid: F,
        elsize: usize,
    ) -> Result<Self, RomIoError>
    where
        F: Fn(&RomData, Offset) -> Result<bool, RomIoError>,
    {
        let mut curr_offset = table_offset;
        let mut length = 0;

        while is_valid(rom_data, curr_offset)? {
            // Increment the length
            length += 1;
            // Increment the offset
            curr_offset += elsize;
        }

        // Find the references to the table_offset
        let references = rom_data.find_references(table_offset, 4);

        Ok(Self {
            offset: table_offset,
            length,
            references,
        })
    }

    /// Repoints a table if needed and returns the new [`RomTable`].
    ///
    /// Updates all references in the ROM.
    ///
    /// Has no special behavior for what the final element should be.
    /// All extra spaces are initialized to 0x00.
    pub fn simple_resize(
        self,
        rom_data: &mut RomData,
        new_length: usize,
        elsize: usize,
    ) -> Result<Self, RomIoError> {
        let old_size = self.length * elsize;
        let new_size = new_length * elsize;

        // Copy the minimum of the two sizes from the start of the offset
        let min_size = old_size.min(new_size);
        let copy = rom_data.read_slice(self.offset, min_size)?.to_vec();

        // Repoint the table
        let new_offset = rom_data.repoint_offset(self.offset, old_size, new_size)?;
        // Allocate the new space (fill it with 0s)
        rom_data.allocate(new_offset, new_size)?;

        // Write the copied data
        rom_data.write_slice(new_offset, &copy)?;

        // Update all the references
        for reference in self.references.iter() {
            rom_data.write_offset(*reference, new_offset)?;
        }

        Ok(Self {
            offset: new_offset,
            length: new_length,
            references: self.references,
        })
    }

    /// Reads all elements of a table into a vector.
    ///
    /// Uses the given function to read the element.
    pub fn read_elements<F, T>(
        &self,
        rom: &Rom,
        read: F,
        elsize: usize,
    ) -> Result<Vec<T>, RomIoError>
    where
        F: Fn(&RomData, Offset) -> Result<T, RomIoError>,
    {
        let mut elements = Vec::with_capacity(self.length);

        let mut curr_offset = self.offset;
        for _ in 0..self.length {
            elements.push(read(&rom.data, curr_offset)?);
            curr_offset += elsize;
        }

        Ok(elements)
    }
}
