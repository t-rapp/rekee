//----------------------------------------------------------------------------
// File: main.rs
// $Id$
//----------------------------------------------------------------------------

#![allow(dead_code)]

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct Coordinate {
    q: i32,
    r: i32,
}

impl Coordinate {
    pub fn new(q: i32, r: i32) -> Self {
        Coordinate { q, r }
    }

    pub fn q(&self) -> i32 {
        self.q
    }

    pub fn r(&self) -> i32 {
        self.r
    }

    pub fn s(&self) -> i32 {
        -self.q - self.r
    }

    pub fn neighbor<T>(&self, dir: T) -> Self 
        where T: Into<Direction>
    {
        match dir.into() {
            Direction::A => Coordinate::new(self.q + 1, self.r),
            Direction::B => Coordinate::new(self.q + 1, self.r - 1),
            Direction::C => Coordinate::new(self.q, self.r - 1),
            Direction::D => Coordinate::new(self.q - 1, self.r),
            Direction::E => Coordinate::new(self.q - 1, self.r + 1),
            Direction::F => Coordinate::new(self.q, self.r + 1),
        }
    }
}

//----------------------------------------------------------------------------

enum Direction {
    A,
    B,
    C,
    D,
    E,
    F,
}

impl From<u8> for Direction {
    fn from(value: u8) -> Direction {
        match value % 6 {
            0 => Direction::A,
            1 => Direction::B,
            2 => Direction::C,
            3 => Direction::D,
            4 => Direction::E,
            5 => Direction::F,
            _ => panic!(),
        }
    }
}

impl From<Direction> for u8 {
    fn from(value: Direction) -> u8 {
        match value {
            Direction::A => 0,
            Direction::B => 1,
            Direction::C => 2,
            Direction::D => 3,
            Direction::E => 4,
            Direction::F => 5,
        }
    }
}

//----------------------------------------------------------------------------

struct Orientation {
    f0: f32,
    f1: f32,
    f2: f32,
    f3: f32,
    b0: f32,
    b1: f32,
    b2: f32,
    b3: f32,
    start_angle: f32,  // in multiples of 60°
}

const SQRT_3: f32 = 1.73205080756887729352744634150587237f32;

const LAYOUT_POINTY: Orientation = Orientation {
    f0: SQRT_3, f1: SQRT_3 / 2.0, f2: 0.0, f3: 3.0 / 2.0,
    b0: SQRT_3 / 3.0, b1: -1.0 / 3.0, b2: 0.0, b3: 2.0 / 3.0,
    start_angle: 0.5,
};

const LAYOUT_FLAT: Orientation = Orientation {
    f0: 3.0 / 2.0, f1: 0.0, f2: SQRT_3 / 2.0, f3: SQRT_3,
    b0: 2.0 / 3.0, b1: 0.0, b2: -1.0 / 3.0, b3: SQRT_3 / 3.0,
    start_angle: 0.0,
};

type Point = (f32, f32);

struct Layout {
    orientation: Orientation,
    size: Point,
    origin: Point,
}

//----------------------------------------------------------------------------

fn main() {
    println!("Hello, world!");
}

//----------------------------------------------------------------------------
