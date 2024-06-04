use aoc_utils::{
    file_utils::read_lines,
    geometry::{self, Point},
};
use std::{collections::HashSet, path::Path};

fn exists(vec: &Vec<Point>, set: &HashSet<Point>) -> bool {
    for point in vec {
        if set.contains(point) {
            return true;
        };
    }
    false
}

pub fn resolve<P>(part: u8, filename: P) -> isize
where
    P: AsRef<Path>,
{
    let mut set: HashSet<Point> = HashSet::new();
    let mut max_asteroids = 0;
    let mut station = &Default::default();
    let mut asteroids_set = HashSet::new();
    let mut row = 0;
    read_lines(filename)
        .unwrap()
        .map(|line| line.unwrap())
        .for_each(|line| {
            line.chars().enumerate().for_each(|(col, char)| {
                if char == '#' {
                    let _ = set.insert(Point::new(row, col as isize));
                }
            });
            row += 1;
        });
    set.iter().for_each(|coord1| {
        let mut asteroids: HashSet<Point> = HashSet::new();
        set.iter().for_each(|coord2| {
            if coord1 != coord2 {
                let line = geometry::Line::new(*coord1, *coord2);
                let vec = line.grid_intersections();
                if !exists(&vec, &set) {
                    let _ = asteroids.insert(*coord2);
                }
            }
        });
        if asteroids.len() > max_asteroids {
            station = coord1;
            max_asteroids = asteroids.len();
            asteroids_set = asteroids;
        }
    });
    if part == 1 {
        max_asteroids as isize
    } else {
        let mut asteroids = asteroids_set.iter().collect::<Vec<&Point>>();
        asteroids.sort_unstable_by(|a1, a2| {
            station
                .north_cw_angle_to(a1)
                .angle()
                .partial_cmp(&station.north_cw_angle_to(a2).angle())
                .unwrap()
        });
        match asteroids.get(199) {
            Some(asteroid) => asteroid.col * 100 + asteroid.row,
            None => {
                println!("No 200th asteroid");
                0
            }
        }
    }
}
