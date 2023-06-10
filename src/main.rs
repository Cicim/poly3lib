
use gba_structs::GBAType;
use poly3lib::GBAType;

#[derive(GBAType)]
pub struct Point {
    pub x: u32,
    pub y: u32,
}

impl Default for Point {
    fn default() -> Self {
        Self {
            x: Default::default(),
            y: Default::default(),
        }
    }
}

fn main() {


}

// fn open_rom() -> Rom {
//     // Get the ROM name from arguments
//     let args: Vec<String> = std::env::args().collect();
//     let rom_name = &args.get(1);

//     if rom_name.is_none() {
//         println!("Usage: poly3 <rom>");
//         exit(1);
//     }
//     let rom_name = rom_name.unwrap();

//     // Load the ROM
//     let rom = Rom::load(rom_name);

//     match rom {
//         Ok(rom) => {
//             println!("ROM loaded successfully!");
//             println!("ROM type: {:?}", rom.rom_type);
//             println!(
//                 "ROM size: {} bytes ({} MB)",
//                 rom.size(),
//                 rom.size() / 1024 / 1024
//             );

//             return rom;
//         }
//         Err(err) => match err {
//             poly3lib::rom::RomError::InvalidRom => {
//                 println!("Invalid ROM file");
//                 exit(2);
//             }
//             poly3lib::rom::RomError::IoError(err) => {
//                 println!("IO error: {}", err);
//                 exit(3);
//             }
//         },
//     };
// }
