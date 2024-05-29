use std::str::FromStr;

#[derive(Default, Debug, PartialEq, Copy, Clone)]
pub struct Point {
    pub row: isize,
    pub col: isize,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParsePointError;

impl Point {
    pub fn new(row: isize, col: isize) -> Self {
        Self { row, col }
    }

    pub fn update(&self, drow: isize, dcol: isize) -> Self {
        Point {
            row: self.row + drow,
            col: self.col + dcol,
        }
    }

    pub fn manhattan_distance(&self, other: &Self) -> usize {
        ((self.row - other.row).abs() + (self.col - other.col).abs()) as usize
    }
}

impl FromStr for Point {
    type Err = ParsePointError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (row, col) = s
            .strip_prefix('(')
            .and_then(|s| s.strip_suffix(')'))
            .and_then(|s| s.split_once(','))
            .ok_or(ParsePointError)?;

        let row_fromstr = row.parse::<isize>().map_err(|_| ParsePointError)?;
        let col_fromstr = col.parse::<isize>().map_err(|_| ParsePointError)?;

        Ok(Point {
            row: row_fromstr,
            col: col_fromstr,
        })
    }
}
