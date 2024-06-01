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
        intcode.set(1, 12);
        intcode.set(2, 2);
        let _ = intcode.intcode_loop();
        intcode.get(0)
    } else {
        for noun in 0..100 {
            for verb in 0..100 {
                let mut intcode_copy = intcode.clone();
                intcode_copy.set(1, noun);
                intcode_copy.set(2, verb);
                let _ = intcode_copy.intcode_loop();
                if intcode_copy.get(0) == 19690720 {
                    return noun * 100 + verb;
                }
            }
        }
        -1
    }
}
