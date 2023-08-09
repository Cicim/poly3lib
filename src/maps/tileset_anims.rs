use std::collections::HashMap;

use gba_types::GBAIOError;
use thiserror::Error;
use thumb::{FunctionCallAction, LoggedEvent, MemoryAccessAction};

use crate::{
    graphics::Graphic,
    rom::{Rom, RomType},
};

use super::{render::TilesetsPair, tileset::TilesetData};

#[derive(Debug, Error)]
pub enum TilesetAnimationError {
    #[error("Invalid tileset animations header offset")]
    InvalidHeaderOffset,

    #[error("The animations field the tileset header is not a callback")]
    NotACallback,
    #[error("Execution error: ")]
    ExecutionError(#[from] thumb::ExecutionError),

    #[error("IO Error: {0}")]
    IoError(#[from] GBAIOError),
}

/// Information about a single tileset animation.
///
/// Any special information that can be obtained about this
/// animation needs to be specified here.
#[derive(Debug)]
pub struct TilesetAnimation {
    /// The graphics needed to render the animations
    ///
    /// The size in tiles of each is the same and it is
    /// equal to the size of the animation.
    pub frame_graphics: Vec<Graphic>,

    /// Time in frames before the animation frame starts
    pub start_time: u16,
    /// The time in frames in-between animation frames
    pub interval: u16,
    /// The tile in the tileset the animation replaces
    pub start_tile: u16,
}

/// A list of animations for a single tileset.
#[derive(Debug)]
pub struct TilesetAnimationList {
    /// Where the animation counter starts for these animations
    pub start_frame: u16,
    /// Where the animation counter resets for these animations
    pub max_frames: u16,

    /// The list of animations with graphics and timings
    pub animations: Vec<TilesetAnimation>,
}

impl TilesetsPair {
    /// Load animations for both tilesets in this pair.
    pub fn load_animations(&mut self, rom: &Rom) -> Result<(), TilesetAnimationError> {
        let res1 = self.primary.load_animations(rom);
        let res2 = self.secondary.load_animations(rom);

        res1.and(res2)
    }
}

impl TilesetData {
    /// Loads the animations for this tileset into the `animations` field.
    fn load_animations(&mut self, rom: &Rom) -> Result<(), TilesetAnimationError> {
        // Get the offset to the animation list
        let header_offset = match self.header.animations.offset() {
            Some(ptr) => ptr,
            None => return Err(TilesetAnimationError::InvalidHeaderOffset),
        };

        // TODO Check if a patch is installed for this ROM
        let anims = load_anims_from_init_function(rom, header_offset)?;
        self.animations = Some(anims);

        Ok(())
    }
}

/// Loads a tileset's animations by simulating them being run on a CPU.
///
/// This is done by running the callback function that is stored in the tileset's header.
fn load_anims_from_init_function(
    rom: &Rom,
    offset: usize,
) -> Result<TilesetAnimationList, TilesetAnimationError> {
    if offset & 1 == 0 {
        return Err(TilesetAnimationError::NotACallback);
    }
    let init_function = offset as u32 + 0x08_000_000;

    let mut cpu = rom
        .get_cpu()
        .add_memory_access_action(0x03, MemoryAccessAction::LogWrite)
        .add_function_call_action(0x0806ff04, FunctionCallAction::Log);

    cpu.run_function(init_function)?;

    // Get the three written values you expect
    let mut start_frame = 0;
    let mut max_frames = 0;
    let mut callback = 0;

    for event in cpu.logged_events.iter() {
        match event {
            LoggedEvent::HalfwordWritten(_, h) => {
                start_frame = max_frames;
                max_frames = *h
            }
            LoggedEvent::WordWritten(_, w) => callback = *w,
            _ => {}
        }
    }

    // Start creating the animations list
    Ok(TilesetAnimationList {
        max_frames,
        start_frame,
        animations: load_anim_list_from_callback(rom, callback, max_frames)?,
    })
}

/// Loads the animation list by repeatedly calling the given function and
/// logging whenever the `AppendTilesetAnimToBuffer` is written to.
fn load_anim_list_from_callback(
    rom: &Rom,
    callback: u32,
    max_count: u16,
) -> Result<Vec<TilesetAnimation>, TilesetAnimationError> {
    let append_tileset_anim_to_buffer_offset: u32 = match rom.rom_type {
        RomType::FireRed | RomType::LeafGreen => 0x0806ff04,
        RomType::Ruby => 0x08072e24,
        RomType::Sapphire => 0x08072e28,
        RomType::Emerald => 0x080a0980,
    };

    // REVIEW Possibly move this stuff to a separate module
    let __umodsi3_offset: u32 = match rom.rom_type {
        RomType::FireRed => 0x081e4684,
        RomType::LeafGreen => 0x0081e4660,
        RomType::Ruby => 0x081e0f08,
        RomType::Sapphire => 0x081e0e98,
        RomType::Emerald => 0x082e7be0,
    };
    let __udivsi3_offset: u32 = match rom.rom_type {
        RomType::FireRed => 0x081e460c,
        RomType::LeafGreen => 0x081e45e8,
        RomType::Ruby => 0x081e0e90,
        RomType::Sapphire => 0x081e0e20,
        RomType::Emerald => 0x082e7b68,
    };

    let mut cpu = rom
        .get_cpu()
        .add_function_call_action(
            __umodsi3_offset,
            FunctionCallAction::RunInstead(|cpu| {
                let a = cpu.get_register(0);
                let b = cpu.get_register(1);
                cpu.set_register(0, a % b);
                cpu.set_register(1, a / b);
                Ok(())
            }),
        )
        .add_function_call_action(
            __udivsi3_offset,
            FunctionCallAction::RunInstead(|cpu| {
                let a = cpu.get_register(0);
                let b = cpu.get_register(1);
                cpu.set_register(0, a / b);
                cpu.set_register(1, a % b);
                Ok(())
            }),
        )
        .add_function_call_action(
            append_tileset_anim_to_buffer_offset,
            FunctionCallAction::LogAndSkip,
        )
        .log_all_function_calls();

    #[derive(Default, Debug)]
    struct FrameChange {
        timer: u16,
        frame: u16,
        tile: u16,
        size: u16,
        graphics_offset: u32,
    }

    let mut animation_changes: HashMap<u32, Vec<FrameChange>> = HashMap::new();
    let mut next_frame = FrameChange::default();
    let mut current_animation_id = 0;

    // Simulate the update of the tile
    for timer in 0..max_count {
        cpu.set_register(0, timer as u32);
        let _ = cpu.run_function(callback + 1);
        for event in cpu.logged_events.iter() {
            match event {
                LoggedEvent::FunctionCalled(addr, r) => {
                    let addr = *addr;

                    // First, the function for `QueueAnimTiles_*` is called
                    if addr != append_tileset_anim_to_buffer_offset {
                        // We use it to register the animation identifier
                        // and the current internal frame number (even though this may be useless)
                        let frame = r[0] as u16;
                        next_frame.timer = timer;
                        next_frame.frame = frame;
                        current_animation_id = addr;

                        if !animation_changes.contains_key(&addr) {
                            animation_changes.insert(addr, vec![]);
                        }
                    }
                    // Then there is a call to AppendTilesetAnimToBuffer
                    else {
                        // The first argument is the indexed graphics offset
                        // This is read from an array we have no visibility into for now.
                        let graphics = r[0];
                        // The second argument is where in the VRAM the data from the
                        // previous offset should be copied, which we can use to get
                        // the tile number.
                        let tile = (r[1] - 0x06_000_000) / 32;
                        // Third argument is the size of the tiles to copy, which we can
                        // use to get the size of the animation.
                        let size = r[2] / 32;

                        next_frame.graphics_offset = graphics;
                        next_frame.tile = tile as u16;
                        next_frame.size = size as u16;

                        // We can now add the frame to the list
                        match animation_changes.get_mut(&current_animation_id) {
                            Some(vec) => vec.push(next_frame),
                            None => {}
                        }

                        // And reset the next frame
                        current_animation_id = 0;
                        next_frame = FrameChange::default();
                    }
                }
                _ => unreachable!(),
            }
        }
        cpu.logged_events.clear();
    }

    // Extract the animations from the observed changes
    let mut anims = vec![];
    for (_, v) in animation_changes {
        // Frames are already sorted by timer
        let first = match v.get(0) {
            None => continue,
            Some(x) => x,
        };
        // Get the second one to compute the duration
        let second = match v.get(1) {
            None => continue,
            Some(x) => x,
        };

        // The size should not change, neither should the tile
        let size = first.size;
        let start_tile = first.tile;
        // Get the first change's frame to get the start offset
        let start_time = first.timer;
        // Get the second change's frame to get the interval
        let interval = second.timer - first.timer;

        let mut gfx_offsets = vec![];
        for frame in v {
            let gfx = frame.graphics_offset - 0x08_000_000;
            // Stop once they start repeating
            if Some(gfx) == gfx_offsets.get(0).cloned() {
                break;
            }
            gfx_offsets.push(gfx);
        }

        let mut frame_graphics = vec![];
        for offset in gfx_offsets {
            // Read the graphics at that offset with the given size
            let gfx = Graphic::read(rom, offset as usize, Some(size as usize))?;
            frame_graphics.push(gfx);
        }

        anims.push(TilesetAnimation {
            frame_graphics,
            start_tile,
            start_time,
            interval,
        });
    }

    Ok(anims)
}
