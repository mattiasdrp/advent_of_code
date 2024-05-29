use aoc_utils::geometry::{Line, Point};
use std::fs;
use std::path::Path;

fn parse(move_str: &str, prev_point: &Point) -> Point {
    let delta = move_str[1..].parse::<isize>().unwrap();
    match move_str.as_bytes()[0] {
        b'R' => prev_point.update(delta, 0),
        b'L' => prev_point.update(-delta, 0),
        b'U' => prev_point.update(0, -delta),
        b'D' => prev_point.update(0, delta),
        c => panic!("{c} is not a proper direction"),
    }
}

fn parse_line(line: &str) -> Vec<Line> {
    let mut veclines: Vec<Line> = vec![];
    let _ = line.split(',').fold(Point::default(), |prev_point, v| {
        let new_point = parse(v, &prev_point);
        let line = Line::new(prev_point, new_point);
        veclines.push(line);
        new_point
    });
    veclines
}

fn resolve1(lines1: &[Line], lines2: &[Line]) -> isize {
    let mut result = std::usize::MAX;
    for line1 in lines1.iter() {
        for line2 in lines2.iter() {
            match line1.grid_intersection(line2) {
                Some(point) if point != Point::default() => {
                    result = result.min(point.manhattan_distance(&Point::default()));
                }
                _ => (),
            }
        }
    }
    result as isize
}

fn resolve2(lines1: &[Line], lines2: &[Line]) -> isize {
    let mut total_steps = usize::MAX;
    lines1.iter().fold(0, |curr_steps1, line1| {
        lines2.iter().fold(0, |curr_steps2, line2| {
            match line1.grid_intersection(line2) {
                Some(point) if point != Point::default() => {
                    // steps from 0 to the current intersection point through the first wire
                    let steps1 = curr_steps1 + point.manhattan_distance(&line1.start);
                    // steps from 0 to the current intersection point through the second wire
                    let steps2 = curr_steps2 + point.manhattan_distance(&line2.start);
                    total_steps = total_steps.min(steps1 + steps2);
                }
                _ => (),
            }
            // increase the steps to reach 0 by the length of line2
            curr_steps2 + line2.manhattan_length()
        });
        // increase the steps to reach 0 by the length of line1
        curr_steps1 + line1.manhattan_length()
    });
    total_steps.try_into().unwrap()
}

pub fn resolve<P>(part: u8, filename: P) -> isize
where
    P: AsRef<Path>,
{
    let lines = fs::read_to_string(&filename).unwrap();
    let lines: Vec<&str> = lines.split_whitespace().collect();
    let lines1 = parse_line(lines[0]);
    let lines2 = parse_line(lines[1]);

    if part == 1 {
        resolve1(&lines1, &lines2)
    } else {
        resolve2(&lines1, &lines2)
    }
}
