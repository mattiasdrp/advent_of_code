use crate::intcode;
use crate::intcode::IntCode;
use std::fs;
use std::path::Path;

pub fn resolve<P>(part: u8, filename: P, input: isize) -> isize
where
    P: AsRef<Path>,
{
    let string = fs::read_to_string(&filename).unwrap();
    let mut intcode: IntCode = IntCode::new(&string);
    if part == 1 {
        intcode.set_input(1);
    } else if part == 2 {
        intcode.set_input(5);
    } else {
        intcode.set_input(input);
    }
    if let intcode::State::Halted = intcode.intcode_loop() {
        intcode.get_output()
    } else {
        panic!("Intcode should have ended on Halted state")
    }
}
