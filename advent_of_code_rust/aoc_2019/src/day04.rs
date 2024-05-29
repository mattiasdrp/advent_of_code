use std::fs;
use std::path::Path;

fn check(number: usize) -> bool {
    let mut num = number;
    let base = 10usize;
    let mut prev = 10;
    let mut double = false;
    while num != 0 {
        let digit = num % base;
        if digit > prev {
            return false;
        };
        double = double || prev == digit;
        prev = digit;
        num /= base;
    }
    double
}

fn unique_check(number: usize) -> bool {
    let mut num = number;
    let base = 10usize;
    let mut prev = 10;
    let mut prevprev = 10;
    let mut double = false;
    while num != 0 {
        let digit = num % base;
        if digit > prev {
            return false;
        };
        double = double || (prev != (num / base) % base && prev == digit && digit != prevprev);
        prevprev = prev;
        prev = digit;
        num /= base;
    }
    double
}

pub fn resolve<P>(part: u8, filename: P) -> isize
where
    P: AsRef<Path>,
{
    let check = if part == 1 { check } else { unique_check };
    if let [low, high] = fs::read_to_string(&filename)
        .unwrap()
        .trim()
        .split('-')
        .map(|v| v.parse::<usize>().unwrap())
        .collect::<Vec<_>>()[..]
    {
        println!("{low} - {high}");
        (low..=high).fold(0, |acc, number| if check(number) { acc + 1 } else { acc })
    } else {
        panic!("bad representation of a range");
    }
}
