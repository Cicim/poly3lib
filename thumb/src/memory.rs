/// Memory of a GBA processor
pub(crate) struct Memory<'rom> {
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
    rom: &'rom [u8],

    /// `0E000000-0E00FFFF` Game Pak SRAM (max 64 KBytes) - 8bit Bus width
    sram: Vec<u8>,
}

impl<'rom> Memory<'rom> {
    /// Create a new memory
    pub fn init(rom: &'rom [u8]) -> Self {
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
}
