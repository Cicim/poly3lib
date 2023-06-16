use std::{fmt::Display, collections::HashMap};

use serde::{Deserialize, Serialize};

use crate::rom::Rom;

#[derive(Debug, Serialize, Deserialize)]
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

#[derive(Debug, Serialize, Deserialize, Default)]
pub struct Refs {
    /// The table containing each group of map headers.
    pub map_groups: Option<TablePointer>,
    /// The list of map header groups pointed to by map_groups.
    pub map_groups_list: Option<Vec<TablePointer>>,
    /// The offsets to each map layout in the ROM.
    pub map_layouts_table: Option<TablePointer>,
    /// Tilesets table with the relative size and whether it is secondary.
    pub tilesets_table: Option<HashMap<usize, (usize, bool)>>,
}

impl Display for Refs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use colored::Colorize;

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

        if let Some(table) = &self.map_layouts_table {
            writeln!(f, "  map_layouts_table: {}", table)?;
        }

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

impl Rom {
    /// Look for all references to pointer in the ROM
    /// and return the offsets to each one.
    pub fn find_references(&self, pointer: usize) -> Vec<usize> {
        let mut references = Vec::new();

        // Make sure the pointer is valid
        if pointer > self.size() {
            return references;
        }

        // Convert the given pointer to a bytearray
        let pointer: u32 = pointer as u32 + 0x08000000;
        let pointer: [u8; 4] = pointer.to_le_bytes();

        // Search for the pointer in the ROM
        // They are always 4 bytes aligned
        for i in (0..self.size()).step_by(4) {
            if self.data[i..i + 4] == pointer {
                references.push(i);
            }
        }

        references
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TableInitError {
    NotImplemented,
    InvalidTablePointer,
    TableGoesOutOfBounds,
}

impl Refs {
    /// Return the map and map groups vectors if present.
    pub fn get_map_groups(&self) -> Option<(&TablePointer, &Vec<TablePointer>)> {
        if let Some(map_groups) = &self.map_groups {
            if let Some(groups_list) = &self.map_groups_list {
                return Some((map_groups, groups_list));
            }
        }

        None
    }

    /// Return the map layouts table if present.
    pub fn get_map_layouts_table(&self) -> Option<&TablePointer> {
        self.map_layouts_table.as_ref()
    }

    /// Return the tilesets table if present.
    pub fn get_tilesets_table(&self) -> Option<&HashMap<usize, (usize, bool)>> {
        self.tilesets_table.as_ref()
    }

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

