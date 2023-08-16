use gba_macro::gba_struct;
use gba_types::GBAIOError;
use serde::Serialize;

use crate::rom::Rom;

use super::ObjectEventError;

// If the sprite has an array of images, this is the array index.
// If the sprite has a sheet, this is the tile offset.
gba_struct!(AnimFrameCmd {
    u32 image_value:16;
    u32 duration:6;
    u32 hflip:1;
    u32 vflip:1;
});

#[derive(Debug, Serialize)]
pub enum AnimCmd {
    /// Sets the image for this frame
    Frame(AnimFrameCmd),

    /// Repeats everything until now for the given number of frames
    ///
    /// Type is `-3` (`0xFFFD`)
    Loop(u8),
    /// Jumps to the given position in the animation.
    ///
    /// Type is `-2` (`0xFFFE`)
    Jump(u8),
    /// Ends the animation.
    ///
    /// Type is `-1` (`0xFFFF`)
    End,
}

impl AnimCmd {
    /// Reads a single animation command
    pub fn read(rom: &Rom, offset: usize) -> Result<Self, GBAIOError> {
        // Read the type
        let cmd_type: i16 = rom.read(offset)?;

        match cmd_type {
            // End of animation (no data)
            -1 => Ok(AnimCmd::End),
            // struct AnimJumpCmd {
            //     u32 type:16;
            //     u32 target:6;
            // };
            -2 => {
                let target: u8 = rom.read(offset + 2)?;
                let target = target & 0x3F;
                Ok(AnimCmd::Jump(target))
            }
            // struct AnimLoopCmd {
            //     u32 type:16;
            //     u32 count:6;
            // };
            -3 => {
                let count: u8 = rom.read(offset + 2)?;
                let count = count & 0x3F;
                Ok(AnimCmd::Loop(count))
            }

            // Read from `AnimFrameCmd`
            _ => {
                let cmd: AnimFrameCmd = rom.read(offset)?;
                Ok(AnimCmd::Frame(cmd))
            }
        }
    }
}

/// Table of animation commands. Ends with End or Jump.
#[derive(Debug, Serialize)]
pub struct ObjectEventAnim(Vec<AnimCmd>);

impl ObjectEventAnim {
    /// Reads a table of animation commands
    pub fn read(rom: &Rom, offset: usize) -> Result<Self, GBAIOError> {
        let mut cmds = Vec::new();

        // Hard limit to avoid infinite loops
        for i in 0..64 {
            let cmd = AnimCmd::read(rom, offset + i * 4)?;
            cmds.push(cmd);

            match cmds.last() {
                Some(AnimCmd::End) | Some(AnimCmd::Jump(_)) => break,
                None => break,
                _ => {}
            }
        }

        Ok(Self(cmds))
    }

    /// Returns how this animation starts.
    pub fn get_first_frame(&self) -> Result<&AnimFrameCmd, ObjectEventError> {
        match self.0.first() {
            Some(AnimCmd::Frame(cmd)) => Ok(cmd),
            _ => Err(ObjectEventError::AnimDoesNotStartWithFrameCmd),
        }
    }
}

impl std::fmt::Display for ObjectEventAnim {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use colored::Colorize;

        for cmd in &self.0 {
            write!(f, "    ")?;

            match cmd {
                AnimCmd::Frame(cmd) => {
                    let command = "ANIMCMD_FRAME".yellow();

                    let image_value = format!("{}", cmd.image_value).purple();
                    let duration = format!("{}", cmd.duration).purple();

                    let hflip = if cmd.hflip == 1 {
                        format!(", .hFlip = {}", "TRUE".yellow())
                    } else {
                        "".to_string()
                    };

                    let vflip = if cmd.vflip == 1 {
                        format!(", .vFlip = {}", "TRUE".yellow())
                    } else {
                        "".to_string()
                    };

                    writeln!(
                        f,
                        "{}({}, {}{}{})",
                        command, image_value, duration, hflip, vflip
                    )?;
                }
                AnimCmd::Loop(count) => {
                    let command = "ANIMCMD_LOOP".yellow();
                    let count = format!("{}", count).purple();
                    writeln!(f, "{}({})", command, count)?;
                }
                AnimCmd::Jump(target) => {
                    let command = "ANIMCMD_JUMP".yellow();
                    let target = format!("{}", target).purple();
                    writeln!(f, "{}({})", command, target)?;
                }
                AnimCmd::End => {
                    writeln!(f, "{}", "ANIMCMD_END".yellow())?;
                }
            }
        }

        Ok(())
    }
}
