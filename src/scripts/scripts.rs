// use std::collections::HashSet;

// use gba_types::GBAIOError;

// use crate::rom::Rom;

// /// This function takes as an argument a list of script offsets
// /// which you are unsure whether you can delete or not.
// ///
// /// It will look for the references to these scripts and if they
// /// are not referenced anywhere else, it will delete them.
// pub fn clear_all_scripts_safely(rom: &mut Rom, scripts: Vec<usize>) -> Result<(), GBAIOError> {
//     // If there are no scripts to clear, we can return early.
//     if scripts.len() == 0 {
//         return Ok(());
//     }

//     // First, we get a view of all the offsets in the ROM.
//     // (this is a slow operation, so we can only do it once)
//     let offsets = rom.find_all_offsets();

//     println!("Now we're going to clear {:#08x?}", scripts);

//     // We care about the number of times a script is referenced, so we cannot use a set.
//     // We will use a hashset to store the scripts we have already explored.
//     let explored_scripts: HashSet<usize> = HashSet::new();

//     Ok(())
// }
