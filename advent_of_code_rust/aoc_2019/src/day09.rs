use crate::intcode::IntCode;
use std::fs;
use std::path::Path;

pub fn resolve<P>(part: u8, filename: P) -> isize
where
    P: AsRef<Path>,
{
    let _ = part;
    let string = fs::read_to_string(&filename).unwrap();
    let mut intcode: IntCode = IntCode::new(&string);
    if part == 1 {
        intcode.set_input(1)
    } else {
        intcode.set_input(2)
    }
    intcode.intcode_loop(false);
    0
}
