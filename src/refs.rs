use std::{collections::HashMap, fmt::Display};

use serde::{Deserialize, Serialize};
use thiserror::Error;

use gba_types::GBAIOError;

use crate::rom::Rom;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TablePointer {
    /// The offset to the table in the ROM.
    pub offset: usize,
    /// The number of entries in the table.
    pub size: usize,
    /// The places where that offset is referenced (if any).
    pub references: Vec<usize>,
}

impl Display for TablePointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use colored::Colorize;

        let offset = format!("0x{:X}", self.offset);

        write!(f, "[{}]@{}", self.size.to_string().red(), offset.blue())?;
        if !self.references.is_empty() {
            write!(f, " <- ")?;
            for (i, reference) in self.references.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                let reference = format!("0x{:X}", reference).green();
                write!(f, "{}", reference)?;
            }
        }

        Ok(())
    }
}

impl TablePointer {
    /// Returns a new [`TablePointer`] with a new offset and size.
    /// Overwrites the offset in all references in the given ROM.
    pub fn update(
        self,
        rom: &mut Rom,
        new_offset: usize,
        new_size: usize,
    ) -> Result<TablePointer, GBAIOError> {
        let mut pointer = self.clone();
        pointer.size = new_size;
        pointer.offset = new_offset;

        // Update the offset
        for reference in &pointer.references {
            rom.write_ptr(*reference, new_offset)?;
        }

        Ok(pointer)
    }
}

#[derive(Debug, Serialize, Deserialize, Default, Clone)]
pub struct Refs {
    /// The table containing each group of map headers.
    pub map_groups: Option<TablePointer>,
    /// The list of map header groups pointed to by map_groups.
    pub map_groups_list: Option<Vec<TablePointer>>,
    /// The offsets to each map layout in the ROM.
    pub map_layouts_table: Option<TablePointer>,
    /// Tilesets table with the relative size and whether it is secondary.
    pub tilesets_table: Option<HashMap<usize, (usize, bool)>>,
    /// The table of map section names.
    pub mapsec_name_table: Option<TablePointer>,
    /// The table of wild encounters.
    pub wilds_table: Option<TablePointer>,

    /// The table of object events graphics.
    pub object_event_gfx_table: Option<TablePointer>,
    /// The table of object event sprite palettes
    pub object_event_pal_table: Option<TablePointer>,
}

impl Display for Refs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use colored::Colorize;

        macro_rules! write_field {
            ($name:ident, $f:ident, $value:expr) => {
                if let Some(table) = &$value {
                    writeln!($f, "  {}: {}", stringify!($name), table)?;
                }
            };
        }

        // If every field is None
        if self.map_groups.is_none() && self.map_layouts_table.is_none() {
            return writeln!(f, "No references found");
        }

        writeln!(f, "Table References:")?;

        if let Some(groups) = &self.map_groups {
            if let Some(list) = &self.map_groups_list {
                writeln!(f, "  map_groups{}:", groups)?;
                for (i, bank) in list.iter().enumerate() {
                    writeln!(f, "    {}: {}", format!("{:<3}", i).red(), bank)?;
                }
            }
        }

        write_field!(map_layouts_table, f, self.map_layouts_table);
        write_field!(mapsec_name_table, f, self.mapsec_name_table);
        write_field!(wilds_table, f, self.wilds_table);
        write_field!(object_event_gfx_table, f, self.object_event_gfx_table);

        if let Some(table) = &self.tilesets_table {
            writeln!(f, "  tilesets_table:")?;
            // Sort the hashmap by offset
            let mut table: Vec<_> = table.iter().collect();
            table.sort_by(|a, b| a.0.cmp(b.0));

            for (offset, (size, is_secondary)) in table.iter() {
                let offset = format!("0x{:X}", offset).blue();
                let size = format!("{:<3}", size).red();
                let primary = if *is_secondary {
                    "secondary".yellow()
                } else {
                    "primary".green()
                };
                writeln!(f, "    tileset {} size {} is {}", offset, size, primary)?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Error)]
pub enum TableInitError {
    #[error("Not implemented for this ROM type")]
    NotImplemented,
    #[error("Invalid table pointer")]
    InvalidTablePointer,
    #[error("Table goes out of bounds")]
    TableGoesOutOfBounds,
}

impl Refs {
    /// Returns the size in blocks of the requested tileset
    pub fn get_tileset_size(&self, tileset: usize) -> Option<usize> {
        if let Some(table) = &self.tilesets_table {
            if let Some((size, _)) = table.get(&tileset) {
                return Some(*size);
            }
        }

        None
    }
}
