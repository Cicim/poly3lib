#[cfg(test)]
mod vector_tests {
    use gba_macro::gba_struct;
    use gba_types::{vectors::VectorData, GBAType, GBAIOError};

    gba_struct!(SimpleVector {
        u32 size;
        u8 data{$size};
    });

    gba_struct!(ComplexVector {
        u16 half_size;
        u16 other_half_size;

        u8 data{$half_size + $other_half_size};
    });

    gba_struct!(ComplexTypeVector {
        u32 size;
        u8 data[3]{$size};
    });

    #[test]
    fn read_simple_vector() {
        let test = [
            0x02u8, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x08, 0x0A, 0x0B, 0x00, 0x00,
        ];

        let m = SimpleVector::read_from(&test, 0).unwrap();
        assert!(matches!(
            m.data,
            gba_types::vectors::VectorData::Valid { .. }
        ));

        if let VectorData::Valid {
            data,
            offset,
            read_length,
        } = m.data
        {
            assert_eq!(data.len(), 2);
            assert_eq!(data[0], 0x0A);
            assert_eq!(data[1], 0x0B);

            assert_eq!(offset, 8);
            assert_eq!(read_length, 2);
        }
    }

    #[test]
    fn read_complex_vector() {
        let test = [
            0x02u8, 0x00, 0x03u8, 0x00, 0x08, 0x00, 0x00, 0x08, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
        ];

        let m = ComplexVector::read_from(&test, 0).unwrap();
        assert!(matches!(
            m.data,
            gba_types::vectors::VectorData::Valid { .. }
        ));

        if let VectorData::Valid {
            data,
            offset,
            read_length,
        } = m.data
        {
            assert_eq!(data.len(), 5);
            assert_eq!(data[0], 0x0A);
            assert_eq!(data[1], 0x0B);
            assert_eq!(data[2], 0x0C);
            assert_eq!(data[3], 0x0D);
            assert_eq!(data[4], 0x0E);

            assert_eq!(offset, 8);
            assert_eq!(read_length, 5);
        }
    }

    #[test]
    fn read_complex_type_vector() {
        let test = [
            0x03u8, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x08, 0x0A, 0x0B, 0x0C, 0x01, 0x02, 0x03,
            0x0D, 0x0E, 0x0F,
        ];

        let m = ComplexTypeVector::read_from(&test, 0).unwrap();
        assert!(matches!(
            m.data,
            gba_types::vectors::VectorData::Valid { .. }
        ));

        if let VectorData::Valid {
            data,
            offset,
            read_length,
        } = m.data
        {
            assert_eq!(data.len(), 3);

            assert_eq!(data[0][0], 0x0A);
            assert_eq!(data[0][1], 0x0B);
            assert_eq!(data[0][2], 0x0C);
            assert_eq!(data[1][0], 0x01);
            assert_eq!(data[1][1], 0x02);
            assert_eq!(data[1][2], 0x03);
            assert_eq!(data[2][0], 0x0D);
            assert_eq!(data[2][1], 0x0E);
            assert_eq!(data[2][2], 0x0F);

            assert_eq!(offset, 8);
            assert_eq!(read_length, 3);
        }
    }

    #[test]
    fn write_new_simple_vector() {
        let mut test = [0xFF; 16];
        let vector = SimpleVector {
            size: 2,
            data: VectorData::New(vec![0x0A, 0x0B]),
        };

        vector.write_to(&mut test, 0).unwrap();

        assert_eq!(
            test,
            [
                0x02u8, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x08, 0x0A, 0x0B, 0xFF, 0xFF, 0xFF,
                0xFF, 0xFF, 0xFF,
            ]
        );
    }

    #[test]
    fn write_by_repointing_vector() {
        // Start from the previous case
        let mut test = [
            0x02u8, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x08, 0x0A, 0x0B, 0xFF, 0xFF, 0xFF, 0xFF,
            0xFF, 0xFF,
        ];

        let mut vector = SimpleVector::read_from(&test, 0).unwrap();
        assert_eq!(
            vector,
            SimpleVector {
                size: 2,
                data: VectorData::Valid {
                    data: vec![0x0A, 0x0B],
                    offset: 8,
                    read_length: 2,
                }
            }
        );

        if let VectorData::Valid {
            data: _,
            offset,
            read_length,
        } = vector.data
        {
            vector.data = VectorData::Valid {
                data: [1, 2, 3, 4, 5, 6, 7, 8].to_vec(),
                offset,
                read_length,
            };

            vector.write_to(&mut test, 0).unwrap();
            assert_eq!(
                test,
                [
                    0x02u8, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x08, 0x01, 0x02, 0x03, 0x04, 0x05,
                    0x06, 0x07, 0x08,
                ]
            );
        }
    }

    #[test]
    fn write_clear_and_null() {
        // Start from the previous case
        let mut test = [
            0x02u8, 0x00, 0x00, 0x00, 0x08, 0x00, 0x00, 0x08, 0x0A, 0x0B, 0xFF, 0xFF, 0xFF, 0xFF,
            0xFF, 0xFF,
        ];

        let mut vector = SimpleVector::read_from(&test, 0).unwrap();
        assert_eq!(
            vector,
            SimpleVector {
                size: 2,
                data: VectorData::Valid {
                    data: vec![0x0A, 0x0B],
                    offset: 8,
                    read_length: 2,
                }
            }
        );

        vector.data = vector.data.to_clear().unwrap();

        let output = [
            0x02u8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
            0xFF, 0xFF,
        ];
        vector.write_to(&mut test, 0).unwrap();
        assert_eq!(test, output);

        vector.data = VectorData::Null;
        vector.write_to(&mut test, 0).unwrap();
        assert_eq!(test, output);
    }


    #[test]
    fn not_enough_space() {
        let mut test = [0xFF; 16];
        let vector = SimpleVector {
            size: 2,
            data: VectorData::New(vec![0xA1; 10]),
        };

        let res = vector.write_to(&mut test, 0);
        assert!(res.is_err());
        assert_eq!(res.unwrap_err(), GBAIOError::RepointingError);
    }
}
