use std::{
    collections::HashMap,
    fmt::Display,
    hash::{Hash, Hasher},
};

use colored::{Color, Colorize};

use rom_data::{Offset, RomIoError};

use crate::Rom;

use super::{ScriptResource, ScriptResourceMarker};

/// Association between read resources and their content.
pub struct ScriptGraph {
    /// The roots for this tree.
    pub(self) roots: Vec<ScriptResourceMarker>,

    /// The resources that have been read in this tree with their type.
    pub(self) read: HashMap<ScriptResourceMarker, ScriptResource>,
    /// The resources referenced by each read script resource.
    pub(self) referenced: HashMap<ScriptResourceMarker, Vec<ScriptResourceMarker>>,
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

    /// Clears these resources from the ROM if all references
    /// for them are contained in the given list.
    ///
    /// Assumes the offsets for the root resources have already been cleared
    pub fn clear(self, rom: &mut Rom) -> Result<(), RomIoError> {
        // Get all the offsets in the ROM.
        let refcounts = rom.data.find_all_offsets();
        self.clear_internal(rom, refcounts)
    }

    /// Clears these resources from the ROM if all references
    /// for them are contained in the given list.
    ///
    /// Assumes the root references have not yet been cleared, but deletes
    /// the root scripts and their resources if needed.
    pub fn clear_ignore_roots(self, rom: &mut Rom) -> Result<(), RomIoError> {
        // Get all the offsets in the ROM.
        let mut refcounts = rom.data.find_all_offsets();
        // For each root, decrease the refcount
        for root in self.roots.iter() {
            match refcounts.get_mut(&root.offset()) {
                Some(rc) => {
                    if *rc > 0 {
                        *rc -= 1
                    }
                }
                None => continue,
            }
        }

        self.clear_internal(rom, refcounts)
    }

    /// Clears these resources from the ROM if all references
    /// for them are contained in the given list.
    fn clear_internal(
        self,
        rom: &mut Rom,
        refcounts: HashMap<Offset, u32>,
    ) -> Result<(), RomIoError> {
        // Decrease the reference count for each resource.
        let mut marker_to_refcount: HashMap<ScriptResourceMarker, u32> =
            HashMap::from_iter(self.referenced.keys().map(|marker| {
                let value = refcounts.get(&marker.offset()).copied().unwrap_or(0);
                (*marker, value)
            }));

        loop {
            // While there are resources with a reference count of 0,
            // clear them and decrease the reference count of the
            // resources that are referenced by them.
            let maybe_marker_with_no_refs = marker_to_refcount
                .iter()
                .find(|(_, refcount)| **refcount == 0)
                .map(|(marker, _)| *marker);

            let res_to_clear = match maybe_marker_with_no_refs {
                Some(marker) => marker,
                None => break,
            };

            #[cfg(feature = "debug_scripts_clear")]
            println!("clearing {}", res_to_clear);

            // Find all attached resources and clear them
            let references = self.referenced.get(&res_to_clear).unwrap();
            for reference in references.iter() {
                // And decrease them
                match marker_to_refcount.get_mut(reference) {
                    Some(rc) => {
                        #[cfg(feature = "debug_scripts_clear")]
                        if *rc == 1 {
                            println!("  refs {}  is next", reference);
                        } else {
                            println!("  refs {}  dec to {}", reference, *rc - 1);
                        }
                        if *rc > 0 {
                            *rc -= 1
                        }
                    }
                    None => continue,
                }
            }

            // Delete it from the map
            marker_to_refcount.remove_entry(&res_to_clear);
            // Clear the parent resource
            let res_to_clear = self.read.get(&res_to_clear).unwrap();
            rom.data
                .clear_bytes(res_to_clear.offset, res_to_clear.size)?;
        }

        Ok(())
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
