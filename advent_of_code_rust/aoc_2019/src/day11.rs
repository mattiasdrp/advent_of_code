use crate::intcode::{IntCode, State};
use aoc_utils::geometry::Point;
use std::collections::HashMap;
use std::fs;
use std::path::Path;

#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone)]
enum Dir {
    Up,
    Left,
    Right,
    Down,
}

impl Dir {
    fn rotate(&self, angle: u8) -> Self {
        match (self, angle) {
            (Self::Up, 1) => Self::Right,
            (Self::Up, 0) => Self::Left,

            (Self::Left, 1) => Self::Up,
            (Self::Left, 0) => Self::Down,

            (Self::Down, 1) => Self::Left,
            (Self::Down, 0) => Self::Right,

            (Self::Right, 1) => Self::Down,
            (Self::Right, 0) => Self::Up,

            _ => panic!("Impossible {:?}", angle),
        }
    }
}

#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone)]
struct Robot {
    position: Point,
    direction: Dir,
}

impl Robot {
    #[inline]
    fn move_up(&mut self) {
        self.position.update(-1, 0)
    }
    #[inline]
    fn move_down(&mut self) {
        self.position.update(1, 0)
    }
    #[inline]
    fn move_left(&mut self) {
        self.position.update(0, -1)
    }
    #[inline]
    fn move_right(&mut self) {
        self.position.update(0, 1)
    }

    fn update(&mut self) {
        match self.direction {
            Dir::Up => self.move_up(),
            Dir::Down => self.move_down(),
            Dir::Right => self.move_right(),
            Dir::Left => self.move_left(),
        }
    }

    fn rotate(&mut self, angle: u8) {
        self.direction = self.direction.rotate(angle);
    }

    fn new() -> Self {
        Self {
            position: Point::default(),
            direction: Dir::Up,
        }
    }
}

pub fn resolve<P>(part: u8, filename: P) -> isize
where
    P: AsRef<Path>,
{
    let string = fs::read_to_string(&filename).unwrap();
    let mut intcode: IntCode = IntCode::new(&string);
    let mut grid = HashMap::new();
    if part == 2 {
        let _ = grid.insert(Point::default(), 1);
    };
    let mut robot = Robot::new();
    let mut painted = false;

    let mut min_row = isize::MAX;
    let mut min_col = isize::MAX;
    let mut max_row = isize::MIN;
    let mut max_col = isize::MIN;

    let mut update_coords = |position: Point| {
        let row = position.row;
        let col = position.col;
        if row < min_row {
            min_row = row
        }
        if row > max_row {
            max_row = row
        }
        if col < min_col {
            min_col = col
        }
        if col > max_col {
            max_col = col
        }
    };

    let res = loop {
        match intcode.intcode_loop(true) {
            State::Output(out) => {
                if painted {
                    robot.rotate(out as u8);
                    robot.update();
                    update_coords(robot.position);
                } else {
                    let _ = grid.insert(robot.position, out);
                };
                painted = !painted
            }

            State::WaitingInput => {
                let input = match grid.get(&robot.position) {
                    None => 0,
                    Some(color) => *color,
                };
                intcode.set_input(input)
            }

            State::Halted => break grid.len() as isize,

            State::Running => (),
        }
    };
    if part == 2 {
        for row in min_row..=max_row {
            for col in min_col..=max_col {
                print!(
                    "{}",
                    match grid.get(&Point::new(row, col)) {
                        Some(1) => "██",
                        _ => "  ",
                    }
                )
            }
            println!()
        }
    };
    res
}
