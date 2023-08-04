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
