use std::fmt::Display;

use colored::Colorize;
use serde::Serialize;

use crate::Rom;

// Sub-modules
pub mod layout;
pub mod map;
pub mod scripts;
pub mod tileset;

impl Rom {
    /// Initializes all the tables related to the maps module from the ROM.
    pub fn init_maps_tables(&mut self) -> ProblemsLog {
        // Keep a hashmap so that you can store which tables failed to initialize
        let mut log = ProblemsLog::new();

        // Initialize the map groups
        map::init_table(self).unwrap_or_else(|e| log.push_error("map_groups", e));

        // Initialize the map layout table
        layout::init_table(self).unwrap_or_else(|e| log.push_error("map_layouts", e));

        // Load the tileset data
        tileset::init_info(self, &mut log).unwrap_or_else(|e| log.push_error("map_tilesets", e));

        // Initialize the scripts data
        scripts::init_table(self, &mut log).unwrap_or_else(|e| log.push_error("scripts", e));

        log
    }
}

// TODO Maybe move to a new file if it is used elsewhere.
#[derive(Serialize)]
pub struct ProblemsLog(Vec<LogMessage>);

#[derive(Debug, Serialize)]
pub(crate) enum LogMessage {
    Warning(&'static str, String),
    Error(&'static str, String),
}

impl ProblemsLog {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    /// Push an error to the struct
    pub fn push_error<T: Display>(&mut self, context: &'static str, error: T) {
        self.0
            .push(LogMessage::Error(context, format!("{}", error)));
    }
    /// Push a warning to the struct
    pub fn push_warning<T: Display>(&mut self, context: &'static str, warning: T) {
        self.0
            .push(LogMessage::Warning(context, format!("{}", warning)))
    }
}

impl Display for ProblemsLog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            writeln!(f, "{}", "No problems found".italic())?;
            return Ok(());
        }

        writeln!(f, "{}:", "Problems".bold().red())?;

        for message in &self.0 {
            match message {
                LogMessage::Warning(context, warning) => {
                    let title = format!("[WARN: {}]", context).yellow();
                    writeln!(f, "  {} {}", title, warning)?
                }
                LogMessage::Error(context, error) => {
                    let title = format!("[ERROR: {}]", context).red();
                    writeln!(f, "  {}: {}", title, error)?
                }
            }
        }

        Ok(())
    }
}
