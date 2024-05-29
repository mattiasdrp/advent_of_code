use crate::utils::read_lines;
use std::{collections::HashMap, path::Path};

pub trait Mass {
    fn required_fuel(self) -> Self;
    fn required_recursive_fuel(self, acc: Self) -> Self;
}

impl Mass for isize {
    #[inline]
    fn required_fuel(self) -> Self {
        (self / 3) - 2
    }

    fn required_recursive_fuel(self, acc: Self) -> Self {
        let sum = self.required_fuel();
        if sum <= 0 {
            acc
        } else {
            sum.required_recursive_fuel(acc + sum)
        }
    }
}

fn resolve1<P>(filename: P) -> isize
where
    P: AsRef<Path>,
{
    read_lines(filename).unwrap().fold(0, |acc, mass| {
        acc + mass.unwrap().parse::<isize>().unwrap().required_fuel()
    })
}

fn resolve2<P>(filename: P) -> isize
where
    P: AsRef<Path>,
{
    let _memo: HashMap<isize, isize> = HashMap::new();
    read_lines(filename).unwrap().fold(0, |acc, mass| {
        acc + mass
            .unwrap()
            .parse::<isize>()
            .unwrap()
            .required_recursive_fuel(0)
    })
}

pub fn resolve<P>(part: u8, filename: P) -> isize
where
    P: AsRef<Path>,
{
    if part == 1 {
        resolve1(filename)
    } else {
        resolve2(filename)
    }
}
