use crate::intcode::{self, IntCode, State};
use aoc_utils::geometry::Point;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

pub fn resolve<P>(part: u8, filename: P) -> isize
where
    P: AsRef<Path>,
{
    let string = fs::read_to_string(&filename).unwrap();
    let mut intcode: IntCode = IntCode::new(&string);

    let mut outputs = [0, 0, 0];
    let mut output_counter = 0;
    let mut grid = HashMap::new();
    if part == 1 {
        let mut blocks = 0;
        loop {
            match intcode.intcode_loop(true) {
                State::Output(out) => {
                    outputs[output_counter] = out;
                    output_counter = (output_counter + 1) % 3;
                    if output_counter == 0 {
                        if out == 2 {
                            blocks += 1
                        };
                        let _ = grid.insert(Point::new(outputs[0], outputs[1]), outputs[2]);
                    };
                }

                State::WaitingInput => (),

                State::Halted => break blocks,

                State::Running => (),
            }
        }
    } else {
        let mut score = 0;
        let mut paddle_x = 0;
        let mut ball_x = 0;
        // Free run setting
        intcode.set(0, 2, intcode::Mode::Position);
        loop {
            match intcode.intcode_loop(true) {
                State::Output(out) => {
                    outputs[output_counter] = out;
                    output_counter = (output_counter + 1) % 3;
                    if output_counter == 0 {
                        if outputs[0] == -1 {
                            score = outputs[2];
                        } else {
                            // paddle initial position
                            if outputs[2] == 3 {
                                paddle_x = outputs[0]
                            };
                            // ball initial position
                            if outputs[2] == 4 {
                                ball_x = outputs[0]
                            };
                            let _ = grid.insert(Point::new(outputs[0], outputs[1]), outputs[2]);
                        }
                    };
                }

                State::WaitingInput => match paddle_x.cmp(&ball_x) {
                    std::cmp::Ordering::Less => intcode.set_input(1),
                    std::cmp::Ordering::Greater => intcode.set_input(-1),
                    std::cmp::Ordering::Equal => intcode.set_input(0),
                },

                State::Halted => break score,

                State::Running => (),
            }
        }
    }
}
