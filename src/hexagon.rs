//----------------------------------------------------------------------------
//! Types and methods for working with a hexagon grid.
//!
//! Much inspiration has been taken from the great
//! [Hexagonal Grids](https://www.redblobgames.com/grids/hexagons/) overview by
//! Red Blob Games.
//
// $Id$
//----------------------------------------------------------------------------

use std::fmt;
use std::ops::{Add, Sub};

//----------------------------------------------------------------------------

/// Coordinate within a hexagon grid.
///
/// The two-dimensional hexagon grid consists of three axes `q`, `r`, and `s`,
/// which are rotated by 60° to each other. A coordinate is stored sparse –
/// `s` is omitted as it can be computed from `q` and `r` on demand.
///
/// # Examples
///
/// ```
/// # use rekee::hexagon::*;
/// let mut pos = Coordinate::new(1, 2);
/// pos = pos + (2, 3).into();
/// assert_eq!(pos.q(), 3);
/// assert_eq!(pos.r(), 5);
/// assert_eq!(pos.s(), -8);
/// ```
#[derive(Debug, Default, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub struct Coordinate {
    q: i32,
    r: i32,
}

impl Coordinate {
    /// Creates a new grid coordinate from positions on `q` and `r` axis.
    pub fn new(q: i32, r: i32) -> Self {
        Coordinate { q, r }
    }

    /// Coordinate position on `q` axis.
    pub fn q(&self) -> i32 {
        self.q
    }

    /// Coordinate position on `r` axis.
    pub fn r(&self) -> i32 {
        self.r
    }

    /// Coordinate position on `s` axis.
    pub fn s(&self) -> i32 {
        -self.q - self.r
    }

    /// Get adjacent grid coordinate based on the given direction.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let pos = Coordinate::new(1, 0);
    /// assert_eq!(pos.neighbor(Direction::A), (2, 0).into());
    /// assert_eq!(pos.neighbor(Direction::B), (1, 1).into());
    /// ```
    pub fn neighbor<T>(&self, dir: T) -> Self
        where T: Into<Direction>
    {
        match dir.into() {
            Direction::A => Coordinate::new(self.q + 1, self.r + 0),
            Direction::B => Coordinate::new(self.q + 0, self.r + 1),
            Direction::C => Coordinate::new(self.q - 1, self.r + 1),
            Direction::D => Coordinate::new(self.q - 1, self.r + 0),
            Direction::E => Coordinate::new(self.q + 0, self.r - 1),
            Direction::F => Coordinate::new(self.q + 1, self.r - 1),
        }
    }

    /// Get coordinate rotated left (counter-clockwise) by 60° around the grid
    /// center.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let pos = Coordinate::new(1, 0);
    /// assert_eq!(pos.rotated_left(), (1, -1).into());
    /// ```
    pub fn rotated_left(&self) -> Self {
        let q = -self.s();
        let r = -self.q();
        Coordinate { q, r }
    }

    /// Get coordinate rotated right (clockwise) by 60° around the grid center.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let pos = Coordinate::new(1, 0);
    /// assert_eq!(pos.rotated_right(), (0, 1).into());
    /// ```
    pub fn rotated_right(&self) -> Self {
        let q = -self.r();
        let r = -self.s();
        Coordinate { q, r }
    }

    /// Convert this hexagon grid coordinate into x/y pixel positions.
    pub fn to_pixel(&self, layout: &Layout) -> Point {
        let o = &layout.orientation;
        let x = (o.f0 * self.q as f32 + o.f1 * self.r as f32) * layout.size.0;
        let y = (o.f2 * self.q as f32 + o.f3 * self.r as f32) * layout.size.1;
        layout.origin + Point(x, y)
    }

    /// Convert x/y pixel positions back into a hexagon grid coordinate.
    pub fn from_pixel_rounded(layout: &Layout, p: Point) -> Self {
        let tmp = FloatCoordinate::from_pixel(layout, p);
        tmp.round()
    }
}

impl fmt::Display for Coordinate {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "({}, {})", self.q, self.r)
    }
}

impl Add for Coordinate {
    type Output = Self;

    fn add(self, other: Coordinate) -> Coordinate {
        Coordinate::new(self.q + other.q, self.r + other.r)
    }
}

impl Sub for Coordinate {
    type Output = Self;

    fn sub(self, other: Coordinate) -> Coordinate {
        Coordinate::new(self.q - other.q, self.r - other.r)
    }
}

impl From<(i32, i32)> for Coordinate {
    fn from(value: (i32, i32)) -> Coordinate {
        Coordinate { q: value.0, r: value.1 }
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

/// Direction within a hexagon grid.
///
/// Used to indicate one of the six possible moving directions within the grid
/// and to specify the rotation angle of hexagon tiles.
///
/// # Examples
///
/// ```
/// # use rekee::hexagon::*;
/// let dir = Direction::A;
/// assert_eq!(dir, 0.into());
/// assert_eq!(dir + Direction::B, Direction::B);
/// assert_eq!(dir - Direction::B, Direction::F);
/// ```
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Direction {
    /// Direction of the positive `q` axis.
    A = 0,
    /// Direction of the positive `r` axis.
    B,
    /// Direction of the positive `s` axis.
    C,
    /// Direction of the negative `q` axis.
    D,
    /// Direction of the negative `r` axis.
    E,
    /// Direction of the negative `s` axis.
    F,
}

impl Direction {
    /// Iterator over all six directions.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let mut dirs = Direction::iter();
    /// assert_eq!(dirs.next(), Some(&Direction::A));
    /// assert_eq!(dirs.next(), Some(&Direction::B));
    /// let mut dirs = dirs.skip(3);
    /// assert_eq!(dirs.next(), Some(&Direction::F));
    /// assert_eq!(dirs.next(), None);
    /// ```
    pub fn iter() -> std::slice::Iter<'static, Self> {
        const DIRS: [Direction; 6] = [Direction::A, Direction::B, Direction::C, Direction::D, Direction::E, Direction::F];
        DIRS.iter()
    }

    /// Get direction rotated left (counter-clockwise).
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let dir = Direction::B;
    /// assert_eq!(dir.rotated_left(), Direction::A);
    ///
    /// let dir = Direction::A;
    /// assert_eq!(dir.rotated_left(), Direction::F);
    /// ```
    pub fn rotated_left(&self) -> Self {
        *self - 1.into()
    }

    /// Get direction rotated right (clockwise).
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let dir = Direction::A;
    /// assert_eq!(dir.rotated_right(), Direction::B);
    ///
    /// let dir = Direction::F;
    /// assert_eq!(dir.rotated_right(), Direction::A);
    /// ```
    pub fn rotated_right(&self) -> Self {
        *self + 1.into()
    }

    /// Convert this hexagon grid direction into an angle in degrees.
    ///
    /// Applies `start_angle` of the given layout. The retuned value is wrapped
    /// within the range of [0 .. 360) degrees.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let layout = Layout::new(Orientation::pointy(), Point(1.0, 1.0), Point(0.0, 0.0));
    ///
    /// let dir = Direction::A;
    /// assert_eq!(dir.to_angle(&layout), 90.0);
    ///
    /// let dir = Direction::B;
    /// assert_eq!(dir.to_angle(&layout), 150.0);
    /// ```
    pub fn to_angle(&self, layout: &Layout) -> f32 {
        let start_angle = layout.orientation.start_angle * 60.0;
        let angle = match self {
            Direction::A => start_angle + 0.0,
            Direction::B => start_angle + 60.0,
            Direction::C => start_angle + 120.0,
            Direction::D => start_angle + 180.0,
            Direction::E => start_angle + 240.0,
            Direction::F => start_angle + 300.0,
        };
        angle.rem_euclid(360.0)
    }
}

impl fmt::Display for Direction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", ('A' as u8 + u8::from(*self)) as char)
    }
}

impl Add for Direction {
    type Output = Self;

    fn add(self, other: Direction) -> Direction {
        let a = u8::from(self);
        let b = u8::from(other);
        (a + b).into()
    }
}

impl Sub for Direction {
    type Output = Self;

    fn sub(self, other: Direction) -> Direction {
        let a = u8::from(self);
        let b = u8::from(other);
        (6 + a - b).into()
    }
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
            _ => unreachable!(),
        }
    }
}

impl From<i8> for Direction {
    fn from(value: i8) -> Direction {
        match value.rem_euclid(6) {
            0 => Direction::A,
            1 => Direction::B,
            2 => Direction::C,
            3 => Direction::D,
            4 => Direction::E,
            5 => Direction::F,
            _ => unreachable!(),
        }
    }
}

impl From<i32> for Direction {
    fn from(value: i32) -> Direction {
        match value.rem_euclid(6) {
            0 => Direction::A,
            1 => Direction::B,
            2 => Direction::C,
            3 => Direction::D,
            4 => Direction::E,
            5 => Direction::F,
            _ => unreachable!(),
        }
    }
}

impl From<Direction> for u8 {
    fn from(value: Direction) -> u8 {
        value as u8
    }
}

impl From<Direction> for i8 {
    fn from(value: Direction) -> i8 {
        value as i8
    }
}

impl From<Direction> for i32 {
    fn from(value: Direction) -> i32 {
        value as i32
    }
}

//----------------------------------------------------------------------------

/// Position within a grid of rectangular pixels.
///
/// # Examples
///
/// ```
/// # use rekee::hexagon::*;
/// let mut pos = Point(1.0, 2.0);
/// pos = pos + (0.4, 1.6).into();
/// assert_eq!(pos.x(), 1.4);
/// assert_eq!(pos.y(), 3.6);
/// ```
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Point(pub f32, pub f32);

impl Point {
    /// Position on the `x` axis.
    pub fn x(&self) -> f32 {
        self.0
    }

    /// Position on the `y` axis.
    pub fn y(&self) -> f32 {
        self.1
    }

    fn distance(&self, other: Point) -> f32 {
        let dx = other.0 - self.0;
        let dy = other.1 - self.1;
        (dx * dx + dy * dy).sqrt()
    }
}

impl Add for Point {
    type Output = Self;

    fn add(self, other: Point) -> Point {
        Point(self.0 + other.0, self.1 + other.1)
    }
}

impl Sub for Point {
    type Output = Self;

    fn sub(self, other: Point) -> Point {
        Point(self.0 - other.0, self.1 - other.1)
    }
}

impl From<(f32, f32)> for Point {
    fn from(value: (f32, f32)) -> Point {
        Point(value.0, value.1)
    }
}

//----------------------------------------------------------------------------

/// Hexagon orientation coefficients.
#[derive(Debug, Clone)]
pub struct Orientation {
    // coefficients for forward conversion of hex coordinates into pixels
    f0: f32,
    f1: f32,
    f2: f32,
    f3: f32,
    // coefficients for backward conversion of pixels into hex coordinates
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
    start_angle: 1.5,
};

const LAYOUT_FLAT: Orientation = Orientation {
    f0: 0.0, f1: 3.0 / 2.0, f2: -SQRT_3, f3: -SQRT_3 / 2.0,
    b0: -1.0 / 3.0, b1: -SQRT_3 / 3.0, b2: 2.0 / 3.0, b3: 0.0,
    start_angle: 0.0,
};

impl Orientation {
    /// Pointy topped orientation, hexagons are aligned in horizonal rows.
    pub fn pointy() -> &'static Self {
        &LAYOUT_POINTY
    }

    /// Flat topped orientation, hexagons are aligned in vertical columns.
    pub fn flat() -> &'static Self {
        &LAYOUT_FLAT
    }
}

//----------------------------------------------------------------------------

/// Coefficients for converting between hexagonal grid coordinates and x/y
/// pixels. Used as parameter for different conversion functions.
#[derive(Debug, Clone)]
pub struct Layout {
    orientation: Orientation,
    size: Point,
    origin: Point,
}

impl Layout {
    /// Creates a new grid layout, consisting of hexagon orientation parameters,
    /// hexagon size in pixels, and the grid origin point.
    pub fn new(orientation: &Orientation, size: Point, origin: Point) -> Self {
        Layout { orientation: orientation.clone(), size, origin }
    }

    /// Returns a reference to the grid orientation parameters.
    pub fn orientation(&self) -> &Orientation {
        &self.orientation
    }

    /// Returns the hexagon size of this grid layout.
    pub fn size(&self) -> Point {
        self.size
    }

    /// Returns the grid origin point.
    pub fn origin(&self) -> Point {
        self.origin
    }

    /// Returns whether the hexagon orientation is "pointy top" or not.
    pub fn is_pointy(&self) -> bool {
        (self.orientation.start_angle - 1.5).abs() <= 0.1
    }

    /// Returns whether the hexagon orientation is "flat top" or not.
    pub fn is_flat(&self) -> bool {
        self.orientation.start_angle.abs() <= 0.1
    }

    /// Calculates the corners of a hexagon with the given coordinate.
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
            corners[i] = Point(center.0 + offset.0, center.1 + offset.1);
        }
        corners
    }
}

impl Default for Layout {
    fn default() -> Self {
        Layout::new(Orientation::pointy(), Point(1.0, 1.0), Point(0.0, 0.0))
    }
}

//----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    const EPS: f32 = 0.001;

    #[test]
    fn coordinate_add() {
        let pos = Coordinate::new(0, 0);
        assert_eq!(Coordinate::new(1, 2), pos + (1, 2).into());
        assert_eq!(Coordinate::new(-2, 3), pos + (-2, 3).into());

        let pos = Coordinate::new(2, 1);
        assert_eq!(Coordinate::new(3, 3), pos + (1, 2).into());
        assert_eq!(Coordinate::new(0, 4), pos + (-2, 3).into());
    }

    #[test]
    fn coordinate_sub() {
        let pos = Coordinate::new(0, 0);
        assert_eq!(Coordinate::new(-1, -2), pos - (1, 2).into());
        assert_eq!(Coordinate::new(2, -3), pos - (-2, 3).into());

        let pos = Coordinate::new(2, 1);
        assert_eq!(Coordinate::new(1, -1), pos - (1, 2).into());
        assert_eq!(Coordinate::new(4, -2), pos - (-2, 3).into());
    }

    #[test]
    fn coordinate_to_pixel() {
        let layout = Layout::new(Orientation::pointy(), Point(10.0, 10.0), Point(0.0, 0.0));
        let p = Coordinate::new(0, 0).to_pixel(&layout);
        assert!(Point(0.0, 0.0).distance(p) < EPS);
        let p = Coordinate::new(0, 1).to_pixel(&layout);
        assert!(Point(8.660, 15.0).distance(p) < EPS);
        let p = Coordinate::new(1, 0).to_pixel(&layout);
        assert!(Point(17.321, 0.0).distance(p) < EPS);
        let p = Coordinate::new(2, -1).to_pixel(&layout);
        assert!(Point(25.981, -15.0).distance(p) < EPS);

        let layout = Layout::new(Orientation::pointy(), Point(20.0, -20.0), Point(0.0, 10.0));
        let p = Coordinate::new(0, 0).to_pixel(&layout);
        assert!(Point(0.0, 10.0).distance(p) < EPS);
        let p = Coordinate::new(0, 1).to_pixel(&layout);
        assert!(Point(17.321, -20.0).distance(p) < EPS);
        let p = Coordinate::new(1, 0).to_pixel(&layout);
        assert!(Point(34.641, 10.0).distance(p) < EPS);
        let p = Coordinate::new(2, -1).to_pixel(&layout);
        assert!(Point(51.962, 40.0).distance(p) < EPS);

        let layout = Layout::new(Orientation::flat(), Point(30.0, 20.0), Point(10.0, 0.0));
        let p = Coordinate::new(0, 0).to_pixel(&layout);
        assert!(Point(10.0, 0.0).distance(p) < EPS);
        let p = Coordinate::new(0, 1).to_pixel(&layout);
        assert!(Point(55.0, -17.321).distance(p) < EPS);
        let p = Coordinate::new(1, 0).to_pixel(&layout);
        assert!(Point(10.0, -34.641).distance(p) < EPS);
        let p = Coordinate::new(2, -1).to_pixel(&layout);
        assert!(Point(-35.0, -51.962).distance(p) < EPS);
    }

    #[test]
    fn coordinate_from_pixel_rounded() {
        let layout = Layout::new(Orientation::pointy(), Point(10.0, 10.0), Point(0.0, 0.0));
        let pos = Coordinate::from_pixel_rounded(&layout, Point(0.0, 0.0));
        assert_eq!(Coordinate::new(0, 0), pos);
        let pos = Coordinate::from_pixel_rounded(&layout, Point(5.0, -5.0));
        assert_eq!(Coordinate::new(0, 0), pos);
        let pos = Coordinate::from_pixel_rounded(&layout, Point(8.0, 15.0));
        assert_eq!(Coordinate::new(0, 1), pos);
        let pos = Coordinate::from_pixel_rounded(&layout, Point(18.0, 1.0));
        assert_eq!(Coordinate::new(1, 0), pos);
        let pos = Coordinate::from_pixel_rounded(&layout, Point(26.0, -16.0));
        assert_eq!(Coordinate::new(2, -1), pos);

        let layout = Layout::new(Orientation::pointy(), Point(20.0, -20.0), Point(0.0, 10.0));
        let pos = Coordinate::from_pixel_rounded(&layout, Point(0.0, 9.0));
        assert_eq!(Coordinate::new(0, 0), pos);
        let pos = Coordinate::from_pixel_rounded(&layout, Point(18.0, -21.0));
        assert_eq!(Coordinate::new(0, 1), pos);
        let pos = Coordinate::from_pixel_rounded(&layout, Point(33.0, 11.0));
        assert_eq!(Coordinate::new(1, 0), pos);
        let pos = Coordinate::from_pixel_rounded(&layout, Point(52.0, 41.0));
        assert_eq!(Coordinate::new(2, -1), pos);

        let layout = Layout::new(Orientation::flat(), Point(30.0, 20.0), Point(10.0, 0.0));
        let pos = Coordinate::from_pixel_rounded(&layout, Point(9.1, 0.5));
        assert_eq!(Coordinate::new(0, 0), pos);
        let pos = Coordinate::from_pixel_rounded(&layout, Point(53.4, -18.2));
        assert_eq!(Coordinate::new(0, 1), pos);
        let pos = Coordinate::from_pixel_rounded(&layout, Point(12.6, -32.3));
        assert_eq!(Coordinate::new(1, 0), pos);
        let pos = Coordinate::from_pixel_rounded(&layout, Point(-36.7, -53.8));
        assert_eq!(Coordinate::new(2, -1), pos);
    }

    #[test]
    fn direction_add() {
        let dir = Direction::A;
        assert_eq!(Direction::B, dir + 1.into());
        assert_eq!(Direction::D, dir + 3.into());

        let dir = Direction::E;
        assert_eq!(Direction::F, dir + 1.into());
        assert_eq!(Direction::B, dir + 3.into());
    }

    #[test]
    fn direction_sub() {
        let dir = Direction::A;
        assert_eq!(Direction::F, dir - 1.into());
        assert_eq!(Direction::D, dir - 3.into());

        let dir = Direction::E;
        assert_eq!(Direction::D, dir - 1.into());
        assert_eq!(Direction::B, dir - 3.into());
    }

    #[test]
    fn direction_to_angle() {
        let layout = Layout::new(Orientation::pointy(), Point(10.0, 10.0), Point(0.0, 0.0));
        let ang = Direction::A.to_angle(&layout);
        assert!((ang - 90.0).abs() < EPS);
        let ang = Direction::B.to_angle(&layout);
        assert!((ang - 150.0).abs() < EPS);
        let ang = Direction::C.to_angle(&layout);
        assert!((ang - 210.0).abs() < EPS);
        let ang = Direction::D.to_angle(&layout);
        assert!((ang - 270.0).abs() < EPS);
        let ang = Direction::E.to_angle(&layout);
        assert!((ang - 330.0).abs() < EPS);
        let ang = Direction::F.to_angle(&layout);
        assert!((ang - 30.0).abs() < EPS);

        let layout = Layout::new(Orientation::pointy(), Point(20.0, -20.0), Point(0.0, 10.0));
        let ang = Direction::from(-1).to_angle(&layout);
        assert!((ang - 30.0).abs() < EPS);
        let ang = Direction::from(0).to_angle(&layout);
        assert!((ang - 90.0).abs() < EPS);
        let ang = Direction::from(1).to_angle(&layout);
        assert!((ang - 150.0).abs() < EPS);
        let ang = Direction::from(2).to_angle(&layout);
        assert!((ang - 210.0).abs() < EPS);
        let ang = Direction::from(3).to_angle(&layout);
        assert!((ang - 270.0).abs() < EPS);
        let ang = Direction::from(4).to_angle(&layout);
        assert!((ang - 330.0).abs() < EPS);
        let ang = Direction::from(5).to_angle(&layout);
        assert!((ang - 30.0).abs() < EPS);
        let ang = Direction::from(6).to_angle(&layout);
        assert!((ang - 90.0).abs() < EPS);

        let layout = Layout::new(Orientation::flat(), Point(30.0, 20.0), Point(10.0, 0.0));
        let ang = Direction::A.to_angle(&layout);
        assert!((ang - 0.0).abs() < EPS);
        let ang = Direction::B.to_angle(&layout);
        assert!((ang - 60.0).abs() < EPS);
        let ang = Direction::C.to_angle(&layout);
        assert!((ang - 120.0).abs() < EPS);
        let ang = Direction::D.to_angle(&layout);
        assert!((ang - 180.0).abs() < EPS);
        let ang = Direction::E.to_angle(&layout);
        assert!((ang - 240.0).abs() < EPS);
        let ang = Direction::F.to_angle(&layout);
        assert!((ang - 300.0).abs() < EPS);
    }

    #[test]
    fn layout_hexagon_corners() {
        let points_to_string = |points: &[Point]| -> String {
            let s: Vec<String> = points.iter()
                .map(|p| format!("({:.1}, {:.1})", p.x(), p.y()))
                .collect();
            s.join(", ")
        };

        let layout = Layout::new(Orientation::pointy(), Point(10.0, 10.0), Point(0.0, 0.0));
        assert_eq!(true, layout.is_pointy());
        assert_eq!(false, layout.is_flat());
        let corners = layout.hexagon_corners((0, 0).into());
        assert_eq!("(-0.0, 10.0), (-8.7, 5.0), (-8.7, -5.0), (0.0, -10.0), (8.7, -5.0), (8.7, 5.0)", points_to_string(&corners));
        let corners = layout.hexagon_corners((0, 1).into());
        assert_eq!("(8.7, 25.0), (-0.0, 20.0), (0.0, 10.0), (8.7, 5.0), (17.3, 10.0), (17.3, 20.0)", points_to_string(&corners));
        let corners = layout.hexagon_corners((1, 0).into());
        assert_eq!("(17.3, 10.0), (8.7, 5.0), (8.7, -5.0), (17.3, -10.0), (26.0, -5.0), (26.0, 5.0)", points_to_string(&corners));
        let corners = layout.hexagon_corners((2, -1).into());
        assert_eq!("(26.0, -5.0), (17.3, -10.0), (17.3, -20.0), (26.0, -25.0), (34.6, -20.0), (34.6, -10.0)", points_to_string(&corners));

        let layout = Layout::new(Orientation::pointy(), Point(20.0, -20.0), Point(0.0, 10.0));
        assert_eq!(true, layout.is_pointy());
        assert_eq!(false, layout.is_flat());
        let corners = layout.hexagon_corners((0, 0).into());
        assert_eq!("(-0.0, -10.0), (-17.3, 0.0), (-17.3, 20.0), (0.0, 30.0), (17.3, 20.0), (17.3, -0.0)", points_to_string(&corners));
        let corners = layout.hexagon_corners((0, 1).into());
        assert_eq!("(17.3, -40.0), (-0.0, -30.0), (0.0, -10.0), (17.3, 0.0), (34.6, -10.0), (34.6, -30.0)", points_to_string(&corners));
        let corners = layout.hexagon_corners((1, 0).into());
        assert_eq!("(34.6, -10.0), (17.3, 0.0), (17.3, 20.0), (34.6, 30.0), (52.0, 20.0), (52.0, -0.0)", points_to_string(&corners));
        let corners = layout.hexagon_corners((2, -1).into());
        assert_eq!("(52.0, 20.0), (34.6, 30.0), (34.6, 50.0), (52.0, 60.0), (69.3, 50.0), (69.3, 30.0)", points_to_string(&corners));

        let layout = Layout::new(Orientation::flat(), Point(30.0, 20.0), Point(10.0, 0.0));
        assert_eq!(false, layout.is_pointy());
        assert_eq!(true, layout.is_flat());
        let corners = layout.hexagon_corners((0, 0).into());
        assert_eq!("(40.0, 0.0), (25.0, 17.3), (-5.0, 17.3), (-20.0, -0.0), (-5.0, -17.3), (25.0, -17.3)", points_to_string(&corners));
        let corners = layout.hexagon_corners((0, 1).into());
        assert_eq!("(85.0, -17.3), (70.0, 0.0), (40.0, 0.0), (25.0, -17.3), (40.0, -34.6), (70.0, -34.6)", points_to_string(&corners));
        let corners = layout.hexagon_corners((1, 0).into());
        assert_eq!("(40.0, -34.6), (25.0, -17.3), (-5.0, -17.3), (-20.0, -34.6), (-5.0, -52.0), (25.0, -52.0)", points_to_string(&corners));
        let corners = layout.hexagon_corners((2, -1).into());
        assert_eq!("(-5.0, -52.0), (-20.0, -34.6), (-50.0, -34.6), (-65.0, -52.0), (-50.0, -69.3), (-20.0, -69.3)", points_to_string(&corners));
    }
}

//----------------------------------------------------------------------------
