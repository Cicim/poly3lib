use thiserror::Error;

/// Memory of a GBA processor
pub struct Memory<'rom> {
    /// `00000000-00003FFF` BIOS - System ROM (16 KBytes)
    bios: Vec<u8>,

    /// `02000000-0203FFFF` WRAM - On-board Work RAM (256 KBytes)
    wram: Vec<u8>,

    /// `03000000-03007FFF` WRAM - On-chip Work RAM (32 KBytes)
    wram_chip: Vec<u8>,

    /// `04000000-040003FE` I/O Registers
    io_registers: Vec<u8>,

    /// `05000000-050003FF` BG/OBJ Palette RAM (1 Kbyte)
    bg_obj_palette_ram: Vec<u8>,

    /// `06000000-06017FFF` VRAM - Video RAM (96 KBytes)
    vram: Vec<u8>,

    /// `07000000-070003FF` OAM - OBJ Attributes (1 Kbyte)
    oam: Vec<u8>,

    /// `08000000-09FFFFFF` Game Pak ROM/FlashROM (max 32MB) - Wait State 0
    pub rom: &'rom [u8],

    /// `0E000000-0E00FFFF` Game Pak SRAM (max 64 KBytes) - 8bit Bus width
    sram: Vec<u8>,
}

#[derive(Debug, Error)]
pub enum MemoryError {
    #[error("Address is in no range: {0:#010X}")]
    InvalidRange(u32),
    #[error("Could not access address: {0:#010X}")]
    InvalidAddress(u32),
    #[error("Attempted to write to ROM")]
    WriteToRom,
    #[error("Unaligned access to address {0:#010X} with alignment {1}")]
    UnalignedAccess(u32, u8),
}

impl<'rom> Memory<'rom> {
    /// Create a new memory
    pub fn new(rom: &'rom [u8]) -> Self {
        Memory {
            bios: vec![],
            wram: vec![0; 256 * 1024],
            wram_chip: vec![0; 32 * 1024],
            io_registers: vec![],
            bg_obj_palette_ram: vec![],
            vram: vec![],
            oam: vec![],
            rom,
            sram: vec![],
        }
    }

    /// Returns an error if the address is unaligned according to the given alignment
    fn ensure_alignment(address: u32, align: u8) -> Result<(), MemoryError> {
        if address % align as u32 == 0 {
            Ok(())
        } else {
            Err(MemoryError::UnalignedAccess(address, align))
        }
    }

    // ANCHOR Byte
    /// Read a byte to an address in memory
    pub fn read_byte(&self, address: u32) -> Result<u8, MemoryError> {
        match address {
            0x00000000..=0x00003FFF => self.bios.get(address as usize - 0x00000000),
            0x02000000..=0x0203FFFF => self.wram.get(address as usize - 0x02000000),
            0x03000000..=0x03007FFF => self.wram_chip.get(address as usize - 0x03000000),
            0x04000000..=0x040003FE => self.io_registers.get(address as usize - 0x04000000),
            0x05000000..=0x050003FF => self.bg_obj_palette_ram.get(address as usize - 0x05000000),
            0x06000000..=0x06017FFF => self.vram.get(address as usize - 0x06000000),
            0x07000000..=0x070003FF => self.oam.get(address as usize - 0x07000000),
            0x08000000..=0x09FFFFFF => self.rom.get(address as usize - 0x08000000),
            0x0E000000..=0x0E00FFFF => self.sram.get(address as usize - 0x0E000000),
            _ => return Err(MemoryError::InvalidRange(address)),
        }
        .ok_or(MemoryError::InvalidAddress(address))
        .map(|&v| v)
    }

    /// Write a byte to an address in memory
    pub fn write_byte(&mut self, address: u32, byte: u8) -> Result<(), MemoryError> {
        let location = match address {
            0x00000000..=0x00003FFF => self.bios.get_mut(address as usize - 0x00000000),
            0x02000000..=0x0203FFFF => self.wram.get_mut(address as usize - 0x02000000),
            0x03000000..=0x03007FFF => self.wram_chip.get_mut(address as usize - 0x03000000),
            0x04000000..=0x040003FE => self.io_registers.get_mut(address as usize - 0x04000000),
            0x05000000..=0x050003FF => self
                .bg_obj_palette_ram
                .get_mut(address as usize - 0x05000000),
            0x06000000..=0x06017FFF => self.vram.get_mut(address as usize - 0x06000000),
            0x07000000..=0x070003FF => self.oam.get_mut(address as usize - 0x07000000),
            0x08000000..=0x09FFFFFF => return Err(MemoryError::WriteToRom),
            0x0E000000..=0x0E00FFFF => self.sram.get_mut(address as usize - 0x0E000000),
            _ => return Err(MemoryError::InvalidRange(address)),
        };

        match location {
            Some(location) => {
                *location = byte;
                Ok(())
            }
            None => Err(MemoryError::InvalidAddress(address)),
        }
    }

    // ANCHOR Halfword
    /// Read a halfword to an address in memory
    pub fn read_halfword(&self, address: u32) -> Result<u16, MemoryError> {
        Memory::ensure_alignment(address, 2)?;

        let byte1 = self.read_byte(address)? as u16;
        let byte2 = self.read_byte(address + 1)? as u16;
        Ok(byte1 | (byte2 << 8))
    }
    /// Write a halfword to an address in memory
    pub fn write_halfword(&mut self, address: u32, halfword: u16) -> Result<(), MemoryError> {
        Memory::ensure_alignment(address, 2)?;

        self.write_byte(address, (halfword & 0xFF) as u8)?;
        self.write_byte(address + 1, (halfword >> 8) as u8)?;
        Ok(())
    }

    // ANCHOR Word
    /// Read a word to an address in memory
    pub fn read_word(&self, address: u32) -> Result<u32, MemoryError> {
        Memory::ensure_alignment(address, 4)?;

        let byte1 = self.read_byte(address)? as u32;
        let byte2 = self.read_byte(address + 1)? as u32;
        let byte3 = self.read_byte(address + 2)? as u32;
        let byte4 = self.read_byte(address + 3)? as u32;
        Ok(byte1 | (byte2 << 8) | (byte3 << 16) | (byte4 << 24))
    }
    /// Write a word to an address in memory
    pub fn write_word(&mut self, address: u32, word: u32) -> Result<(), MemoryError> {
        Memory::ensure_alignment(address, 4)?;

        self.write_byte(address, (word & 0xFF) as u8)?;
        self.write_byte(address + 1, ((word >> 8) & 0xFF) as u8)?;
        self.write_byte(address + 2, ((word >> 16) & 0xFF) as u8)?;
        self.write_byte(address + 3, ((word >> 24) & 0xFF) as u8)?;
        Ok(())
    }
}
