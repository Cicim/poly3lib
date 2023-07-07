use crate::{rom::Rom, scripts::tree::visit_script};

use super::{ScriptResource, ScriptTree};

impl ScriptTree {
    /// Clears a script forest previously read from ROM starting at some root offsets.
    fn clear_internal(self, rom: &mut Rom, roots_references_cleared: bool) {
        // Find all the offsets in the ROM
        let mut offsets = rom.find_all_offsets();

        // Association of resource to dependencies
        let mut tree = self.map;

        if roots_references_cleared {
            // For each root script, decrease its offset from the offsets
            for root in self.roots.iter() {
                let offset = root.offset();
                if let Some(reference) = offsets.get_mut(&(offset as u32)) {
                    *reference -= 1;
                } else {
                    println!("It is highly weird that a root offset does not appear in ROM");
                }
            }
        }

        #[cfg(feature = "debug_scripts_clear")]
        for root in self.roots.iter() {
            let counts = offsets.get(&(root.offset() as u32)).unwrap_or(&0);
            println!("[ROOT COUNT] {} = {}", root, counts);
        }

        // While there is a resource in the tree that does not have any reference in the ROM:
        while let Some(k) = tree
            .iter()
            .map(|(k, _)| k.clone())
            .find(|k| offsets.get(&(k.offset() as u32)).unwrap_or(&0) == &0)
        {
            // Remove k
            let values = tree.remove(&k).unwrap();
            #[cfg(feature = "debug_scripts_clear")]
            println!("[CLEARING] {}", k);
            delete_script_resource(rom, k);

            // Decrease the offsets from all the resources that k references
            for value in values {
                #[cfg(feature = "debug_scripts_clear")]
                {
                    let counts = offsets.get(&(value.offset() as u32)).unwrap_or(&0);
                    print!("    [COUNT] {} = {}", value, counts);
                }

                let counts = offsets.get_mut(&(value.offset() as u32));
                if let Some(count) = counts {
                    if *count > 0 {
                        *count -= 1;
                        #[cfg(feature = "debug_scripts_clear")]
                        print!(" -> {}", count);
                    }
                }

                #[cfg(feature = "debug_scripts_clear")]
                println!()
            }
        }

        #[cfg(feature = "debug_scripts_clear")]
        {
            // Print the remaining offsets
            print!("[NOT CLEARED]");
            for (k, _) in tree.iter() {
                let counts = offsets.get(&(k.offset() as u32)).unwrap_or(&0);
                print!(" {} ({}) ", k, counts);
            }
            println!()
        }
    }
}

impl Rom {
    /// Takes a list of script resources, assuming the places they've been taken
    /// from have been cleared, and finds, then deletes all the resources
    /// that are exclusively referenced by them.
    pub fn clear_scripts(&mut self, resources: Vec<ScriptResource>) {
        let tree = ScriptTree::read(self, resources);
        tree.clear_internal(self, false)
    }
}

fn delete_script_resource(rom: &mut Rom, res: ScriptResource) -> bool {
    use ScriptResource::*;

    match res {
        Script(offset) => {
            // Visit the script by returing the size of each instruction
            match visit_script(rom, offset, |_, bytes| Some(bytes.len() + 1))
                // Sum the results if correct
                .map(|res| res.iter().sum::<usize>())
            {
                // If the visit was successful, clear the script, and return whether the script was cleared
                Ok(script_size) => rom.clear(offset, script_size).is_ok(),
                Err(_) => return false,
            }
        }
        Text(offset) => rom.clear_text(offset).is_ok(),
        Movement(offset) => {
            // Find the first 0xFE after this
            let end = rom.find_byte_after(offset, 0xFE);

            // Clear everything in-between
            if let Some(end) = end {
                // Refuse to delete movements that are too big
                if end - offset > 0x200 {
                    return false;
                }

                rom.clear(offset, end - offset + 1).is_ok()
            } else {
                false
            }
        }
        Products(offset) => {
            let mut size = 0;

            // Read the products until you find a 0x00
            loop {
                let half = rom.read_unaligned_halfword(offset + size);
                size += 2;
                match half {
                    Err(_) => return false,
                    // After ITEM_NONE, clear the end and release (that are there for some reason)
                    Ok(0x0000) => return rom.clear(offset, size + 2).is_ok(),
                    _ => (),
                }
            }
        }
        MapScriptsTable(offset) => {
            let mut read_size = 0;
            loop {
                // Read the first variable
                let first_var = rom.read_unaligned_halfword(offset + read_size);
                if first_var.is_err() {
                    return false;
                }

                read_size += 2;
                if first_var == Ok(0) {
                    break;
                }
                // We don't care about reading the second variable, we're clearing it
                // Nor do we care about the script (if we've read it, we can clear it)
                read_size += 6;
            }

            // Clear the table
            rom.clear(offset, read_size).is_ok()
        }
    }
}
