mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;
mod day13;
mod intcode;
mod utils;
use clap::Parser;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Name of the person to greet
    #[arg(short, long, default_value_t = 1)]
    day: u8,

    /// Number of times to greet
    #[arg(short, long, default_value_t = 1)]
    part: u8,

    #[arg(short, long, default_missing_value = Some(""), num_args = 0..=1)]
    example: Option<String>,

    #[arg(short, long, default_value_t = 1)]
    intcode_input: isize,
}

fn main() {
    let args = Args::parse();
    let input = format!(
        "rsc/day{:02}{}",
        args.day,
        match args.example {
            Some(ref suffix) => format!("-example{suffix}"),
            None => "".to_string(),
        }
    );
    println!(
        "result of day {} part {} file {}",
        args.day, args.part, input
    );
    println!(
        "  is {}",
        match args.day {
            1 => day01::resolve(args.part, input),
            2 => day02::resolve(args.part, input),
            3 => day03::resolve(args.part, input),
            4 => day04::resolve(args.part, input),
            5 => day05::resolve(args.part, input, args.intcode_input),
            6 => day06::resolve(args.part, input),
            7 => day07::resolve(args.part, input),
            8 => day08::resolve(args.part, input, args.example.is_some()),
            9 => day09::resolve(args.part, input),
            10 => day10::resolve(args.part, input),
            11 => day11::resolve(args.part, input),
            12 => day12::resolve(args.part, input),
            13 => day13::resolve(args.part, input),
            _ => todo!(),
        }
    )
}
