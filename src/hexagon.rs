//----------------------------------------------------------------------------
// File: hexagon.rs
// $Id$
//----------------------------------------------------------------------------

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

    pub fn to_pixel(&self, layout: &Layout) -> Point {
        let o = &layout.orientation;
        let x = (o.f0 * self.q as f32 + o.f1 * self.r as f32) * layout.size.0;
        let y = (o.f2 * self.q as f32 + o.f3 * self.r as f32) * layout.size.1;
        (x, y)
    }

    pub fn from_pixel_rounded(layout: &Layout, p: Point) -> Self {
        let tmp = FloatCoordinate::from_pixel(layout, p);
        tmp.round()
    }
}

//----------------------------------------------------------------------------

struct FloatCoordinate {
    q: f32,
    r: f32,
}

impl FloatCoordinate {
    pub fn q(&self) -> f32 {
        self.q
    }

    pub fn r(&self) -> f32 {
        self.r
    }

    pub fn s(&self) -> f32 {
        -self.q - self.r
    }

    pub fn from_pixel(layout: &Layout, p: Point) -> Self {
        let o = &layout.orientation;
        let x = (p.0 - layout.origin.0) / layout.size.0;
        let y = (p.1 - layout.origin.1) / layout.size.1;
        let q = o.b0 * x + o.b1 * y;
        let r = o.b2 * x + o.b3 * y;
        FloatCoordinate { q, r }
    }

    pub fn round(&self) -> Coordinate {
        let mut q = self.q().round();
        let mut r = self.r().round();
        let s = self.s().round();
        let q_diff = (q - self.q()).abs();
        let r_diff = (r - self.r()).abs();
        let s_diff = (s - self.s()).abs();
        if q_diff > r_diff && q_diff > s_diff {
            q = -r - s;
        } else if r_diff > s_diff {
            r = -q - s;
        }
        Coordinate { q: q as i32, r: r as i32 }
    }
}

//----------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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

#[derive(Debug, Clone)]
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

impl Orientation {
    pub fn pointy() -> &'static Self {
        &LAYOUT_POINTY
    }

    pub fn flat() -> &'static Self {
        &LAYOUT_FLAT
    }
}

//----------------------------------------------------------------------------

type Point = (f32, f32);

#[derive(Debug, Clone)]
struct Layout {
    orientation: Orientation,
    size: Point,
    origin: Point,
}

impl Layout {
    pub fn new(orientation: &Orientation, size: Point, origin: Point) -> Self {
        Layout { orientation: orientation.clone(), size, origin }
    }

    pub fn hexagon_corners(&self, hex: Coordinate) -> [Point; 6] {
        let mut corners = [Point::default(); 6];
        let center = hex.to_pixel(&self);
        let corner_offset = |corner: u8| {
            use std::f32::consts::PI;
            let angle = 2.0 * PI * (self.orientation.start_angle + f32::from(corner)) / 6.0;
            let x = self.size.0 * angle.cos();
            let y = self.size.1 * angle.sin();
            (x, y)
        };
        for i in 0..6 {
            let offset = corner_offset(i as u8);
            corners[i] = (center.0 + offset.0, center.1 + offset.1);
        }
        corners
    }
}

//----------------------------------------------------------------------------
