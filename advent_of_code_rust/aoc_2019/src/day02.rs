use crate::intcode;
use crate::intcode::IntCode;
use std::fs;
use std::path::Path;

pub fn resolve<P>(part: u8, filename: P) -> isize
where
    P: AsRef<Path>,
{
    let string = fs::read_to_string(&filename).unwrap();
    let mut intcode: IntCode = IntCode::new(&string);
    if part == 1 {
        intcode.set(1, 12, intcode::Mode::Position);
        intcode.set(2, 2, intcode::Mode::Position);
        let _ = intcode.intcode_loop(false);
        intcode.get(0, intcode::Mode::Position)
    } else {
        for noun in 0..100 {
            for verb in 0..100 {
                let mut intcode_copy = intcode.clone();
                intcode_copy.set(1, noun, intcode::Mode::Position);
                intcode_copy.set(2, verb, intcode::Mode::Position);
                let _ = intcode_copy.intcode_loop(false);
                if intcode_copy.get(0, intcode::Mode::Position) == 19690720 {
                    return noun * 100 + verb;
                }
            }
        }
        -1
    }
}
