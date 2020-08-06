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

fn main() {
    println!("Hello, world!");
}

//----------------------------------------------------------------------------
