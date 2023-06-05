#[cfg(test)]
mod test_rom {
    use poly3lib::rom::*;
    
    const FIRERED_PATH: &str = "tests/roms/firered.gba";

    #[test]
    fn rom_loading() {
        // Load the ROM
        let rom = Rom::load(FIRERED_PATH);
        assert!(rom.is_ok());
        let rom = rom.unwrap();
        
        assert_eq!(rom.data.len(), 0x1000000);
        assert_eq!(rom.rom_type, RomType::FireRed);
    }

    #[test]
    fn rom_saving() {
        // Load the ROM
        let rom = Rom::load(FIRERED_PATH).unwrap();
        // Save the ROM to a temporary file
        let tmp_path = "tests/roms/firered.tmp.gba";
        rom.save(tmp_path).unwrap();
        // Load the temporary ROM
        let tmp_rom = Rom::load(tmp_path).unwrap();
        // Delete the temporary file
        std::fs::remove_file(tmp_path).unwrap();
        // Compare the two ROMs
        assert!(rom.data == tmp_rom.data);
    }

    // Read/Write tests
    #[test]
    fn rom_read() {
        // Load the ROM
        let rom = Rom::load(FIRERED_PATH).unwrap();
        // Read the first 4 bytes
        let bytes = rom.read(0, 4);
        assert!(bytes.is_ok());

        if let Ok(bytes) = bytes {
            assert_eq!(bytes, vec![0x7f, 0x00, 0x00, 0xea]);
        }
    }

    #[test]
    fn rom_read_out_of_bounds() {
        // Load the ROM
        let rom = Rom::load(FIRERED_PATH).unwrap();
        // Read 4 bytes starting at the end of the ROM
        let bytes = rom.read(rom.size(), 4);
        assert!(bytes.is_err());

        if let Err(err) = bytes {
            assert_eq!(err, OutOfBoundsError);
        }
    }

    #[test]
    fn rom_write() {
        // Load the ROM
        let mut rom = Rom::load(FIRERED_PATH).unwrap();
        // Write 4 bytes to the ROM
        let bytes = rom.write(0, &[0xff, 0xff, 0xff, 0xff]);
        assert!(bytes.is_ok());

        if let Ok(()) = bytes {
            // Read the first 4 bytes
            let bytes = rom.read(0, 4);
            assert!(bytes.is_ok());

            if let Ok(bytes) = bytes {
                assert_eq!(bytes, vec![0xff, 0xff, 0xff, 0xff]);
            }
        }
    }

    #[test]
    fn rom_write_out_of_bounds() {
        // Load the ROM
        let mut rom = Rom::load(FIRERED_PATH).unwrap();
        // Write 4 bytes starting at the end of the ROM
        let bytes = rom.write(rom.size(), &[0xff, 0xff, 0xff, 0xff]);
        assert!(bytes.is_err());

        if let Err(err) = bytes {
            assert_eq!(err, OutOfBoundsError);
        }
    }

    
}
