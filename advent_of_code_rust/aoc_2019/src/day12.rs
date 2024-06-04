use regex::Regex;
use std::{fs, path::Path, str::FromStr};

#[derive(Debug, Clone)]
pub struct Vec3 {
    x: isize,
    y: isize,
    z: isize,
}

impl std::fmt::Display for Vec3 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}, {}, {}]", self.x, self.y, self.z)
    }
}

impl From<[isize; 3]> for Vec3 {
    fn from(a: [isize; 3]) -> Self {
        Self {
            x: a[0],
            y: a[1],
            z: a[2],
        }
    }
}

impl Vec3 {
    pub fn new(x: isize, y: isize, z: isize) -> Self {
        Self { x, y, z }
    }

    pub fn zero() -> Self {
        Self::new(0, 0, 0)
    }

    pub fn add_cmp(&mut self, orig: &Self, that: &Self) {
        self.x += that.x.cmp(&orig.x) as isize;
        self.y += that.y.cmp(&orig.y) as isize;
        self.z += that.z.cmp(&orig.z) as isize;
    }

    fn energy(&self) -> isize {
        self.x.abs() + self.y.abs() + self.z.abs()
    }

    fn update(&mut self, other: &Self) {
        self.x += other.x;
        self.y += other.y;
        self.z += other.z;
    }
}

#[derive(Debug)]
struct Moon {
    pos: Vec3,
    vel: Vec3,
}

#[derive(Debug, Clone)]
struct ParseMoonError;

impl FromStr for Moon {
    type Err = ParseMoonError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"<x=(?<x>-?[0-9]+), y=(?<y>-?[0-9]+), z=(?<z>-?[0-9]+)>$").unwrap();
        let Some(capture) = re.captures(s) else {
            return Err(ParseMoonError);
        };
        let a: Vec3 = [
            capture["x"].parse::<isize>().unwrap(),
            capture["y"].parse::<isize>().unwrap(),
            capture["z"].parse::<isize>().unwrap(),
        ]
        .into();
        Ok(Moon::new(a))
    }
}

impl Moon {
    fn new(pos: impl Into<Vec3>) -> Self {
        Self {
            pos: pos.into(),
            vel: Vec3::zero(),
        }
    }

    fn with_vel(&mut self, vel: impl Into<Vec3>) {
        self.vel = vel.into();
    }

    fn pos(&self) -> &Vec3 {
        &self.pos
    }

    fn vel(&self) -> &Vec3 {
        &self.vel
    }

    fn apply_gravity(&mut self, that: &mut Self) {
        self.vel.add_cmp(&self.pos().clone(), that.pos());
        that.vel.add_cmp(&that.pos().clone(), self.pos())
    }

    fn kinetic_energy(&self) -> isize {
        self.pos.energy()
    }

    fn potential_energy(&self) -> isize {
        self.vel.energy()
    }

    fn update(&mut self) {
        self.pos.update(&self.vel);
    }
}

#[derive(Debug)]
struct Moons {
    moons: Vec<Moon>,
}

impl From<Vec<Moon>> for Moons {
    fn from(moons: Vec<Moon>) -> Self {
        Self { moons }
    }
}

impl Moons {
    fn new(moons: impl Into<Vec<Moon>>) -> Self {
        Self {
            moons: moons.into(),
        }
    }

    fn len(&self) -> usize {
        self.moons.len()
    }

    fn apply_gravity(&mut self) {
        for i in 0..self.len() {
            let (left, right) = self.moons.split_at_mut(i + 1);
            let moon1 = left.last_mut().unwrap();
            for moon2 in right {
                moon1.apply_gravity(moon2);
            }
        }
    }
    // fn apply_gravity(&mut self) {
    //     // ignoring last moon ~~~v
    //     for i in 0..self.len() - 1 {
    //         // slice starting at the current moon
    //         let mut slice = self.moons[i..].iter_mut();
    //         // retrieve current moon
    //         if let Some(moon1) = slice.next() {
    //             // work on tail
    //             for moon2 in slice {
    //                 moon1.apply_gravity(moon2)
    //             }
    //         } else {
    //             // unreachable unless we fuck up the indices
    //             unreachable!()
    //         }
    //     }
    // }

    fn moon_loop(&mut self) {
        println!("before gravity {:?}", self.moons);
        self.apply_gravity();
        println!("after gravity {:?}", self.moons);
        for moon in self.moons.iter_mut() {
            moon.update()
        }
        println!("{:?}", self.moons);
    }
}

pub fn resolve<P>(part: u8, filename: P) -> isize
where
    P: AsRef<Path>,
{
    let moons = fs::read_to_string(filename).unwrap();
    let moons: Vec<&str> = moons.split('\n').collect();
    let moon1 = Moon::from_str(moons[0]).unwrap();
    let moon2 = Moon::from_str(moons[1]).unwrap();
    let moon3 = Moon::from_str(moons[2]).unwrap();
    let moon4 = Moon::from_str(moons[3]).unwrap();
    let mut moons = Moons::new([moon1, moon2, moon3, moon4]);
    for _ in 0..1000 {
        moons.moon_loop()
    }
    moons.moons.iter().fold(0, |acc, moon| {
        acc + moon.kinetic_energy() * moon.potential_energy()
    })
}
