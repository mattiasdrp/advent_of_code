use crate::intcode::{IntCode, State};
use aoc_utils::vec_utils;
use std::collections::HashSet;
use std::fs;
use std::path::Path;

pub fn resolve<P>(part: u8, filename: P) -> isize
where
    P: AsRef<Path>,
{
    let string = fs::read_to_string(&filename).unwrap();
    let intcode: IntCode = IntCode::new(&string);

    if part == 1 {
        let phases = vec![0, 1, 2, 3, 4];
        let phasesperm: vec_utils::VecPermutation<isize> = phases.into();
        let mut max_output = 0;
        phasesperm.into_iter().for_each(|phases| {
            let mut output = 0;
            phases.iter().for_each(|phase| {
                let mut intcode_clone = intcode.clone();
                intcode_clone.set_input(*phase);
                intcode_clone.set_input(output);
                intcode_clone.intcode_loop(false);
                output = intcode_clone.get_output()
            });
            max_output = max_output.max(output);
        });
        max_output
    } else {
        let phases = vec![5, 6, 7, 8, 9];
        let phasesperm: vec_utils::VecPermutation<isize> = phases.into();
        let mut max_output = 0;
        phasesperm.into_iter().for_each(|phases| {
            let mut amplifiers = vec![intcode.clone(); 5];
            phases.iter().enumerate().for_each(|(index, phase)| {
                amplifiers[index].set_input(*phase);
                amplifiers[index].set_identifier(index);
            });
            let mut amplifier = &mut amplifiers[0];
            amplifier.set_input(0);
            let mut halted = HashSet::new();
            'exit: loop {
                match amplifier.intcode_loop(true) {
                    State::Halted if halted.len() == 5 => {
                        max_output = max_output.max(amplifiers[4].get_output());
                        break 'exit;
                    }

                    State::Output(out) => {
                        let index = amplifier.get_identifier();
                        let index_next = (amplifier.get_identifier() + 1) % 5;
                        amplifier = &mut amplifiers[index_next];
                        amplifier.set_input(out);
                        amplifier = &mut amplifiers[index];
                    }

                    State::Halted => {
                        halted.insert(amplifier.get_identifier());
                        let index = (amplifier.get_identifier() + 1) % 5;
                        amplifier = &mut amplifiers[index];
                    }

                    State::WaitingInput => {
                        let index = (amplifier.get_identifier() + 1) % 5;
                        amplifier = &mut amplifiers[index];
                    }
                    State::Running => {}
                }
            }
        });
        max_output
    }
}
