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
        intcode.intcode_loop()
    } else {
        for noun in 0..100 {
            for verb in 0..100 {
                let mut intcode_copy = intcode.clone();
                intcode_copy.set(1, noun);
                intcode_copy.set(2, verb);
                if intcode_copy.intcode_loop() == 19690720 {
                    return noun * 100 + verb;
                }
            }
        }
        -1
    }
}
