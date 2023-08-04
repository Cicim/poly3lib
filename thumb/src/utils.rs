/// Returns the registers set in the rlist
pub fn get_registers_in_rlist(rlist: u8) -> Vec<u8> {
    let mut set_to_one = vec![];
    for i in 0..8 {
        if (rlist >> i) & 1 == 1 {
            set_to_one.push(i);
        }
    }
    set_to_one
}

/// Returns the relative offset to jump to given the bit representation
pub fn extend_8bit_offset(offset: u8) -> i32 {
    ((offset as i8) as i32) * 2
}

/// Returns the relative offset to jump to given the 11-bit representation
///
/// # Examples
/// ```norun
/// assert_eq!(extend_11bit_offset(0b00000000000), 0);
/// assert_eq!(extend_11bit_offset(0b01111111111), 2046);
/// assert_eq!(extend_11bit_offset(0b10000000000), -2048);
/// ```

pub fn extend_11bit_offset(offset: u16) -> i32 {
    // Convert the offset to an integer
    let offset = offset as i32;
    // Sign extend it to 32 bits
    let offset = offset << 21;
    let offset = offset >> 21;

    // Multiply it by 2
    offset * 2
}
