use std::{collections::HashMap, fmt::Display};

use colored::Colorize;
use serde::{Deserialize, Serialize};

use rom_data::{Offset, RomBase, RomData, RomFileError, RomIoError};

// ANCHOR Rom struct
/// The main ROM object.
///
/// Every access to its data can be done via its `data` property.
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
#[derive(Default, Serialize, Deserialize, Clone)]
/// The references to the various tables in the ROM.
///
/// Also contains values that are not references to tables.
pub struct RomReferences {
    /// The table of all layouts in the ROM.
    pub map_layouts: Option<RomTable>,

    /// The list of tilesets loaded from the ROM.
    pub map_tilesets: Option<HashMap<Offset, crate::maps::tileset::TilesetShortInfo>>,
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

        Ok(())
    }
}

// ANCHOR RomTable struct
#[derive(Default, Serialize, Deserialize, Clone)]
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
    /// Reads a table while its elements are valid (as returned by `is_valid`).
    ///
    /// Requires the `table_offset`, NOT one of its references.
    pub fn read_table<F>(
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
}
