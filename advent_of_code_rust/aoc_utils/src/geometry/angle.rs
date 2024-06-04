use std::f64::consts::PI;

#[derive(Default, Debug, PartialEq, Copy, Clone)]
pub struct Radiant {
    angle: f64,
}

impl Radiant {
    pub fn new(angle: f64) -> Self {
        Radiant {
            angle: (angle + 2.0 * PI) % (2.0 * PI),
        }
    }

    pub fn angle(&self) -> f64 {
        self.angle
    }

    pub fn to_degrees(&self) -> Degree {
        Degree::from(*self)
    }
}

#[derive(Default, Debug, PartialEq, Copy, Clone)]
pub struct Degree {
    angle: f64,
}

impl Degree {
    pub fn angle(&self) -> f64 {
        self.angle
    }
}

impl From<Radiant> for Degree {
    fn from(rangle: Radiant) -> Self {
        Self {
            angle: (rangle.angle * 180.0) / (PI),
        }
    }
}

impl From<Degree> for Radiant {
    fn from(dangle: Degree) -> Self {
        Self {
            angle: (dangle.angle * PI) / 180.0,
        }
    }
}

impl From<f64> for Radiant {
    fn from(angle: f64) -> Self {
        if !(-2.0 * PI..=4.0 * PI).contains(&angle) {
            println!("You're using an angle {angle} that looks like degrees")
        };
        Self::new(angle)
    }
}
