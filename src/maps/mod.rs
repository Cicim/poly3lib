use std::collections::HashMap;

use rom_data::RomIoError;

use crate::Rom;

// Sub-modules
pub mod layout;

impl Rom {
    /// Initializes all the tables related to the maps module from the ROM.
    pub fn init_maps_tables(&mut self) -> HashMap<&'static str, RomIoError> {
        // Keep a hashmap so that you can store which tables failed to initialize
        let mut errors = HashMap::new();

        // Initialize the map layout table
        layout::init_table(self).unwrap_or_else(|e| {
            errors.insert("map_layouts", e);
        });

        errors
    }
}
