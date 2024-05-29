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

    pub fn grid_intersection(&self, other: &Self) -> Option<Point> {
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

    pub fn manhattan_length(&self) -> usize {
        self.start.manhattan_distance(&self.end)
    }
}
