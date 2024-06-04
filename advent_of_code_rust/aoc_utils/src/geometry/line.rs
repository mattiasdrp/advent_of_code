use crate::algebra;
use crate::geometry::Point;

#[derive(Default, Debug, PartialEq, Copy, Clone)]
pub struct Line {
    pub start: Point,
    pub end: Point,
}

impl Line {
    pub fn new<P: Into<Point>>(start: P, end: P) -> Self {
        Self {
            start: start.into(),
            end: end.into(),
        }
    }

    pub fn grid_line_intersection(&self, other: &Self) -> Option<Point> {
        let num1 = (self.start.col - other.start.col) * (other.end.row - other.start.row)
            - (self.start.row - other.start.row) * (other.end.col - other.start.col);
        let denom1 = (self.end.row - self.start.row) * (other.end.col - other.start.col)
            - (self.end.col - self.start.col) * (other.end.row - other.start.row);
        let r = (num1 as f64) / (denom1 as f64);
        let num2 = (self.start.col - other.start.col) * (self.end.row - self.start.row)
            - (self.start.row - other.start.row) * (self.end.col - self.start.col);
        let denom2 = (self.end.row - self.start.row) * (other.end.col - other.start.col)
            - (self.end.col - self.start.col) * (other.end.row - other.start.row);
        let s = (num2 as f64) / (denom2 as f64);

        if (0. ..=1.).contains(&r) && (0. ..=1.).contains(&s) {
            let row =
                (self.start.row as f64 + r * ((self.end.row - self.start.row) as f64)) as isize;
            let col =
                (self.start.col as f64 + r * ((self.end.col - self.start.col) as f64)) as isize;
            Some(Point { row, col })
        } else {
            None
        }
    }

    fn d_row(&self) -> isize {
        self.start.row - self.end.row
    }
    fn d_col(&self) -> isize {
        self.start.col - self.end.col
    }

    pub fn grid_intersections(&self) -> Vec<Point> {
        let d_row = self.d_row();
        let d_col = self.d_col();
        let d_gcd = algebra::signed_gcd(d_row, d_col) as isize;
        let d_row = d_row / d_gcd;
        let d_col = d_col / d_gcd;
        let mut res = Vec::new();
        let mut row = self.end.row;
        let mut col = self.end.col;
        loop {
            if row == self.start.row && col == self.start.col {
                break;
            };
            row += d_row;
            col += d_col;
            res.push(Point::new(row, col));
        }
        let _ = res.pop();
        res
    }

    pub fn manhattan_length(&self) -> usize {
        self.start.manhattan_distance(&self.end)
    }
}
