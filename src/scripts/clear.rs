use crate::{rom::Rom, scripts::tree::visit_script};

use super::{ScriptResource, ScriptTree};

impl ScriptTree {
    /// Takes a list of offsets, assuming the places they've been taken
    /// from have been cleared, and finds, then deletes all the resources
    /// and scripts that are exclusively referenced by them.
    pub fn clean_clear(rom: &mut Rom, offsets: Vec<usize>) {
        let tree = Self::read(rom, offsets);
        tree.clear_internal(rom, false)
    }

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
                if let Some(reference) = offsets.get_mut(&offset) {
                    *reference -= 1;
                } else {
                    println!("It is highly weird that a root offset does not appear in ROM");
                }
            }
        }

        #[cfg(feature = "debug_scripts_clear")]
        for root in self.roots.iter() {
            let counts = offsets.get(&root.offset()).unwrap_or(&0);
            println!("[ROOT COUNT] {} = {}", root, counts);
        }

        // While there is a resource in the tree that does not have any reference in the ROM:
        while let Some(k) = tree
            .iter()
            .map(|(k, _)| k.clone())
            .find(|k| offsets.get(&k.offset()).unwrap_or(&0) == &0)
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
                    let counts = offsets.get(&value.offset()).unwrap_or(&0);
                    print!("    [COUNT] {} = {}", value, counts);
                }

                let counts = offsets.get_mut(&value.offset());
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
                let counts = offsets.get(&k.offset()).unwrap_or(&0);
                print!(" {} ({}) ", k, counts);
            }
            println!()
        }
    }
}

fn delete_script_resource(rom: &mut Rom, res: ScriptResource) -> bool {
    use ScriptResource::*;

    match res {
        Script(offset) => {
            let offset = offset as usize;

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
        Text(_) => todo!(),
        Movement(offset) => {
            // Find the first 0xFE after this
            let offset = offset as usize;
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
        TrainerBattle(_) => todo!(),
        Products(_) => todo!(),
        InvalidPointer(_) => return false,
    }
}
