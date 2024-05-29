mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
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

    #[arg(short, long, action = clap::ArgAction::SetTrue)]
    example: bool,

    #[arg(short, long, default_value_t = 1)]
    intcode_input: isize,
}

fn main() {
    let args = Args::parse();
    let input = format!(
        "rsc/day{:02}{}",
        args.day,
        if args.example { "-example" } else { "" }
    );
    println!(
        "result of day {} part {} is: {}",
        args.day,
        args.part,
        match args.day {
            1 => day01::resolve(args.part, input),
            2 => day02::resolve(args.part, input),
            3 => day03::resolve(args.part, input),
            4 => day04::resolve(args.part, input),
            5 => day05::resolve(args.part, input, args.intcode_input),
            _ => todo!(),
        }
    )
}
