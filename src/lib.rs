pub mod types;
pub mod tables;

pub mod rom;
pub mod refs;

pub enum GBAReadingError {
    OutOfBounds,
    InvalidData,
}
pub trait GBAType<T> {
    fn write(&self, buffer: &mut [u8], offset: usize) -> Result<(), GBAReadingError>;
    fn read(buffer: &[u8], offset: usize) -> Result<T, GBAReadingError>;
    fn size() -> usize;
}