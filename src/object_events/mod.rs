use gba_types::GBAIOError;

use thiserror::Error;

pub mod anims;
pub mod graphics;
pub mod render;
pub mod subsprites;

#[derive(Debug, Error)]
pub enum ObjectEventError {
    #[error("The object event graphics table has not been initialized!")]
    TableNotInitialized,

    #[error("Invalid graphics id: {0}")]
    InvalidGraphicsId(u8),
    #[error("Could not find palette with tag {0:04x}")]
    PaletteNotFound(u16),
    #[error("Could not find subsprites table for object event graphics")]
    InvalidSubspritesTable,
    #[error("Could not find images for object event graphics")]
    InvalidImages,
    #[error("Could not find animations for object event graphics")]
    InvalidAnimations,
    #[error("Could not find OAM data for object event graphics")]
    InvalidOamData,

    #[error("Invalid animation id {0}")]
    InvalidAnimId(u8),
    #[error("An animation does not start with the AnimFrame command")]
    AnimDoesNotStartWithFrameCmd,
    #[error("Trying to render a frame with an invalid image id {0}")]
    InvalidImageId(u32),

    #[error("IO Error: {0}")]
    IoError(#[from] GBAIOError),
}
