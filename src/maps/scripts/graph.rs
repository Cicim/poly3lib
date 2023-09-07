use std::{
    collections::HashMap,
    fmt::Display,
    hash::{Hash, Hasher},
};

use colored::{Color, Colorize};

use rom_data::Offset;

use crate::Rom;

use super::{ScriptResource, ScriptResourceMarker};

/// Association between read resources and their content.
pub struct ScriptGraph {
    /// The roots for this tree.
    roots: Vec<ScriptResourceMarker>,

    /// The resources that have been read in this tree with their type.
    read: HashMap<ScriptResourceMarker, ScriptResource>,
    /// The resources referenced by each read script resource.
    referenced: HashMap<ScriptResourceMarker, Vec<ScriptResourceMarker>>,
}

impl ScriptGraph {
    /// Reads a [ScriptGraph] starting from the given points.
    pub fn read(rom: &Rom, root_resources: &[ScriptResourceMarker]) -> Self {
        let mut graph = Self {
            roots: Vec::from(root_resources),
            read: HashMap::new(),
            referenced: HashMap::new(),
        };

        // Exhaust all resources in the queue.
        let mut queue = graph.roots.clone();
        while let Some(marker) = queue.pop() {
            // If the resource has already been read, skip it.
            if graph.read.contains_key(&marker) {
                continue;
            }

            // Read the resource.
            let mut refs = Vec::new();
            match marker.read(rom, Some(&mut refs)) {
                Ok(resource) => {
                    // Extend the queue with the found references
                    queue.extend(refs.iter());

                    graph.read.insert(marker, resource);
                    graph.referenced.insert(marker, refs);
                }
                // Ignore errors for now.
                Err(_) => {}
            }
        }

        graph
    }

    /// Sort the keys of the graph.
    fn sort_keys(&self) -> Vec<&ScriptResourceMarker> {
        let mut keys = self.referenced.keys().collect::<Vec<_>>();
        keys.sort_by(|a, b| {
            let offset_a = a.offset();
            let offset_b = b.offset();

            let priority_a = a.priority();
            let priority_b = b.priority();

            let is_root_a = self.roots.contains(a) as u8;
            let is_root_b = self.roots.contains(b) as u8;

            is_root_b
                .cmp(&is_root_a)
                .then(priority_a.cmp(&priority_b))
                .then(offset_a.cmp(&offset_b))
        });

        keys
    }
}

impl Display for ScriptGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Script graph:")?;

        // Print the read resources
        for marker in self.sort_keys() {
            let refs = self.referenced.get(marker).unwrap();

            write!(
                f,
                "  • {}",
                // If this resource is a root, underline it
                if self.roots.contains(marker) {
                    marker.to_string().underline().to_string()
                } else {
                    marker.to_string()
                }
            )?;
            // Print size
            if let Some(resource) = self.read.get(marker) {
                write!(f, " ({} bytes)", resource.size)?;
            }

            if refs.is_empty() {
                writeln!(f)?;
            } else {
                writeln!(
                    f,
                    " => [{}]",
                    refs.iter()
                        .map(|ref_el| {
                            if self.roots.contains(ref_el) {
                                ref_el.to_string().underline().to_string()
                            } else {
                                ref_el.to_string()
                            }
                            .italic()
                            .to_string()
                        })
                        .collect::<Vec<String>>()
                        .join(", ")
                )?;
            }
        }

        Ok(())
    }
}

impl Display for ScriptResourceMarker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (text, offset) = match self {
            ScriptResourceMarker::Script(offset) => ("Scri".bright_green(), offset),
            ScriptResourceMarker::Movement(offset) => ("Move".blue(), offset),
            ScriptResourceMarker::Text(offset) => ("Text".red(), offset),
            ScriptResourceMarker::Products(offset) => ("Prod".bright_blue(), offset),
        };

        write!(f, "{}:{}", text.bold(), color_offset(*offset))
    }
}

const HASH_COLORS: &[Color; 8] = &[
    Color::Green,
    Color::Yellow,
    Color::Magenta,
    Color::Cyan,
    Color::Red,
    Color::BrightMagenta,
    Color::BrightCyan,
    Color::BrightWhite,
];

/// Returns the colors for the given resource.
fn color_offset(offset: Offset) -> String {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    offset.hash(&mut hasher);
    let hash = hasher.finish();

    let digit_12_color = HASH_COLORS[((hash >> 0) & 0b111) as usize];
    let digit_34_color = HASH_COLORS[((hash >> 3) & 0b111) as usize];
    let digit_567_color = HASH_COLORS[((hash >> 9) & 0b111) as usize];

    let digit_12 = format!("{:02X}", offset % 256).color(digit_12_color);
    let digit_34 = format!("{:02X}", (offset >> 8) % 256).color(digit_34_color);
    let digit_567 = format!("{:03X}", (offset >> 16)).color(digit_567_color);

    format!("{}{}{}", digit_567, digit_34, digit_12)
}
