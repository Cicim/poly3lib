#[cfg(test)]
mod struct_tests {
    use gba_macro::gba_struct;
    use gba_types::{
        pointers::{Nothing, PointedData},
        GBAType,
    };

    #[test]
    fn only_ints() {
        gba_struct!(OnlyInts {
            u8 a;
            u16 b;
            u32 c;
            i8 d;
            i16 e;
            i32 f;
        });

        let mut only_ints = OnlyInts::default();
        only_ints.a = 1;
        only_ints.b = 2;
        only_ints.c = 3;
        only_ints.d = -4;
        only_ints.e = -5;
        only_ints.f = -6;

        // Write it
        let mut buffer = [0u8; 16];
        only_ints.write_to(&mut buffer, 0).unwrap();
        assert_eq!(
            buffer,
            [
                0x01, 0x00, 0x02, 0x00, 0x03, 0x00, 0x00, 0x00, 0xFC, 0x00, 0xFB, 0xFF, 0xFA, 0xFF,
                0xFF, 0xFF
            ]
        );

        // Read it
        let reread = OnlyInts::read_from(&buffer, 0).unwrap();
        assert_eq!(only_ints, reread);
    }

    #[test]
    fn void_pointer() {
        gba_struct!(VoidPointer {
            i32 a;
            void* b;
        });

        let mut void_pointer = VoidPointer::default();
        void_pointer.a = -1;
        void_pointer.b = PointedData::Valid(8, Nothing);

        // Write it
        let mut buffer = [0u8; 9];
        void_pointer.write_to(&mut buffer, 0).unwrap();
        assert_eq!(
            buffer,
            [0xFF, 0xFF, 0xFF, 0xFF, 0x08, 0x00, 0x00, 0x08, 0x00,]
        );

        // Read it
        let reread = VoidPointer::read_from(&buffer, 0).unwrap();
        assert_eq!(void_pointer, reread);
    }

    #[test]
    fn bitfields() {
        gba_struct!(Bitfields {
            u8 a: 1;
            u8 b: 2;
            u8 c: 3;
            u8 d: 4;
            i32 e: 5;
            i32 f: 6;
            i32 g: 7;
            i16 h: 8;
        });

        let mut bitfields = Bitfields::default();
        bitfields.a = 1;
        bitfields.b = 2;
        bitfields.c = 3;
        bitfields.d = 4;
        bitfields.e = -5;
        bitfields.f = -6;
        bitfields.g = -7;
        bitfields.h = -8;

        // Write it
        let mut buffer = [0u8; 12];
        bitfields.write_to(&mut buffer, 0).unwrap();
        assert_eq!(buffer, [29, 4, 0, 0, 91, 207, 3, 0, 248, 0, 0, 0]);

        // Read it
        let reread = Bitfields::read_from(&buffer, 0).unwrap();
        assert_eq!(bitfields, reread);
    }

}
