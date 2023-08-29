use crate::RomData;

pub enum RomValueError {}

pub trait RomValues: Sized {
    /// Reads the first reference to these each value.
    ///
    /// Returns even if the ROM is corrupted and some values don't match.
    fn read_fast(rom: &RomData) -> Result<Self, RomValueError>;

    /// Reads all references to to these values, returning the
    /// the final value struct only if they all match.
    fn read_all(rom: &RomData) -> Result<Self, RomValueError>;

    /// Updates all references to these values to the given
    /// value struct.
    fn write(self, rom: &RomData) -> Result<(), RomValueError>;
}

enum RomValueReadWriteMethod {
    LslMov,
    Lsr,
    Word,
    Halfword,
    Byte,
}

impl RomValueReadWriteMethod {
    pub fn get_read_fn() {}

    pub fn get_write_fn() {}
}

enum RomValueTransformation {
    Shift(u8),
    Mask(u16),
}

impl RomValueTransformation {
    pub fn apply_transform() {}

    pub fn invert_transform() {}
}
