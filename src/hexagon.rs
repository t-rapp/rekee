//----------------------------------------------------------------------------
//! Types and methods for working with a hexagon grid.
//!
//! Much inspiration has been taken from the great
//! [Hexagonal Grids](https://www.redblobgames.com/grids/hexagons/) overview by
//! Red Blob Games.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use std::fmt;
use std::ops::{Add, Sub, Mul, Index, IndexMut};

use serde::{Serialize, Deserialize};

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
#[derive(Serialize, Deserialize)]
pub struct Coordinate {
    q: i32,
    r: i32,
}

impl Coordinate {
    /// Creates a new grid coordinate from positions on `q` and `r` axis.
    pub const fn new(q: i32, r: i32) -> Self {
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
    #[allow(clippy::identity_op)]
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
    pub fn to_pixel(self, layout: &Layout) -> Point {
        FloatCoordinate::from(self)
            .to_pixel(layout)
    }

    /// Convert x/y pixel positions back into a hexagon grid coordinate.
    pub fn from_pixel_rounded(layout: &Layout, p: Point) -> Self {
        FloatCoordinate::from_pixel(layout, p)
            .to_coordinate()
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

/// Floating-point variant of a grid [`Coordinate`].
///
/// Mainly used as an intermediate type when converting between x/y pixel
/// positions and hexagon grid coordinates.
#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd)]
#[derive(Serialize, Deserialize)]
pub struct FloatCoordinate {
    q: f32,
    r: f32,
}

impl FloatCoordinate {
    /// Creates a new floating-point grid coordinate from positions on `q` and
    /// `r` axis.
    pub const fn new(q: f32, r: f32) -> Self {
        FloatCoordinate { q, r }
    }

    pub fn q(&self) -> f32 {
        self.q
    }

    pub fn r(&self) -> f32 {
        self.r
    }

    pub fn s(&self) -> f32 {
        -self.q - self.r
    }

    #[cfg(test)]
    fn distance(&self, other: FloatCoordinate) -> f32 {
        let dq = (other.q() - self.q()).abs();
        let dr = (other.r() - self.r()).abs();
        let ds = (other.s() - self.s()).abs();
        dq.max(dr.max(ds))
    }

    /// Rotate this hexagon grid coordinate by the given floating-point direction.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let pos = FloatCoordinate::new(1.0, 0.0)
    ///     .rotate(FloatDirection(-1.0));
    ///
    /// let check = FloatCoordinate::new(1.0, -1.0);
    /// assert!((pos.q() - check.q()).abs() < 1e-6);
    /// assert!((pos.r() - check.r()).abs() < 1e-6);
    /// ```
    pub fn rotate<D>(self, dir: D) -> Self
        where D: Into<FloatDirection>
    {
        let phi = dir.into().to_angle().to_radians();
        let c0 = phi.cos() - SQRT_3 / 3.0 * phi.sin();
        let c1 = -2.0 * SQRT_3 / 3.0 * phi.sin();
        let c2 = 2.0 * SQRT_3 / 3.0 * phi.sin();
        let c3 = phi.cos() + SQRT_3 / 3.0 * phi.sin();
        let q = c0 * self.q + c1 * self.r;
        let r = c2 * self.q + c3 * self.r;
        FloatCoordinate { q, r }
    }

    /// Convert this floating-point grid coordinate into x/y pixel positions.
    pub fn to_pixel(self, layout: &Layout) -> Point {
        let o = &layout.orientation;
        let x = (o.f0 * self.q + o.f1 * self.r) * layout.size.0;
        let y = (o.f2 * self.q + o.f3 * self.r) * layout.size.1;
        layout.origin + Point(x, y)
    }

    /// Convert x/y pixel positions into a floating-point grid coordinate.
    #[allow(clippy::many_single_char_names)]
    pub fn from_pixel(layout: &Layout, p: Point) -> Self {
        let o = &layout.orientation;
        let x = (p.0 - layout.origin.0) / layout.size.0;
        let y = (p.1 - layout.origin.1) / layout.size.1;
        let q = o.b0 * x + o.b1 * y;
        let r = o.b2 * x + o.b3 * y;
        FloatCoordinate { q, r }
    }

    /// Convert this floating-point grid coordinate into the nearest hexagon
    /// grid coordinate.
    pub fn to_coordinate(self) -> Coordinate {
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

impl fmt::Display for FloatCoordinate {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "({}, {})", self.q, self.r)
    }
}

impl Add for FloatCoordinate {
    type Output = Self;

    fn add(self, other: FloatCoordinate) -> FloatCoordinate {
        FloatCoordinate::new(self.q + other.q, self.r + other.r)
    }
}

impl Sub for FloatCoordinate {
    type Output = Self;

    fn sub(self, other: FloatCoordinate) -> FloatCoordinate {
        FloatCoordinate::new(self.q - other.q, self.r - other.r)
    }
}

impl From<(f32, f32)> for FloatCoordinate {
    fn from(value: (f32, f32)) -> FloatCoordinate {
        FloatCoordinate { q: value.0, r: value.1 }
    }
}

impl From<Coordinate> for FloatCoordinate {
    fn from(value: Coordinate) -> FloatCoordinate {
        FloatCoordinate { q: value.q as f32, r: value.r as f32 }
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
#[derive(Serialize, Deserialize)]
#[serde(into = "u8", from = "u8")]
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

    /// Get opposite direction (point reflection).
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let dir = Direction::A;
    /// assert_eq!(dir.opposite(), Direction::D);
    ///
    /// let dir = Direction::F;
    /// assert_eq!(dir.opposite(), Direction::C);
    /// ```
    pub fn opposite(&self) -> Self {
        *self + 3.into()
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
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let dir = Direction::A;
    /// assert_eq!(dir.to_angle(), 0.0);
    ///
    /// let dir = Direction::B;
    /// assert_eq!(dir.to_angle(), 60.0);
    /// ```
    pub fn to_angle(&self) -> f32 {
        match self {
            Direction::A => 0.0,
            Direction::B => 60.0,
            Direction::C => 120.0,
            Direction::D => 180.0,
            Direction::E => 240.0,
            Direction::F => 300.0,
        }
    }
}

impl fmt::Display for Direction {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", (b'A' + u8::from(*self)) as char)
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

impl<T> Index<Direction> for [T; 6] {
    type Output = T;

    fn index(&self, index: Direction) -> &Self::Output {
        self.get(u8::from(index) as usize).unwrap()
    }
}

impl<T> IndexMut<Direction> for [T; 6] {
    fn index_mut(&mut self, index: Direction) -> &mut Self::Output {
        self.get_mut(u8::from(index) as usize).unwrap()
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

/// Floating-point variant of a hexagon [`Direction`].
///
/// Represents a grid direction value as a float number in multiples of 60°.
#[derive(Debug, Default, Clone, Copy, PartialEq)]
#[derive(Serialize, Deserialize)]
pub struct FloatDirection(pub f32);

impl FloatDirection {
    /// Convert this hexagon grid direction into an angle in degrees.
    ///
    /// The retuned value is wrapped within a range of 0° to 360°.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let dir = FloatDirection(0.0);
    /// assert_eq!(dir.to_angle(), 0.0);
    ///
    /// let dir = FloatDirection(1.0);
    /// assert_eq!(dir.to_angle(), 60.0);
    /// ```
    pub fn to_angle(&self) -> f32 {
        let angle = self.0 * 60.0;
        angle.rem_euclid(360.0)
    }

    /// Create a hexagon grid direction from an angle in degrees.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let dir = FloatDirection::from_angle(30.0);
    /// assert_eq!(dir, FloatDirection(0.5));
    ///
    /// let dir = FloatDirection::from_angle(-30.0);
    /// assert_eq!(dir, FloatDirection(5.5));
    /// ```
    pub fn from_angle(value: f32) -> Self {
        let angle = value.rem_euclid(360.0);
        FloatDirection(angle / 60.0)
    }
}

impl fmt::Display for FloatDirection {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", self.0)
    }
}

impl Add for FloatDirection {
    type Output = Self;

    fn add(self, other: FloatDirection) -> FloatDirection {
        FloatDirection(self.0 + other.0)
    }
}

impl Sub for FloatDirection {
    type Output = Self;

    fn sub(self, other: FloatDirection) -> FloatDirection {
        FloatDirection(self.0 - other.0)
    }
}

impl From<f32> for FloatDirection {
    fn from(value: f32) -> FloatDirection {
        FloatDirection(value)
    }
}

impl From<FloatDirection> for f32 {
    fn from(value: FloatDirection) -> f32 {
        value.0
    }
}

impl From<Direction> for FloatDirection {
    fn from(value: Direction) -> FloatDirection {
        FloatDirection(u8::from(value) as f32)
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

    #[cfg(test)]
    fn distance(&self, other: Point) -> f32 {
        let dx = other.0 - self.0;
        let dy = other.1 - self.1;
        (dx * dx + dy * dy).sqrt()
    }
}

impl fmt::Display for Point {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "({}, {})", self.0, self.1)
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

impl Mul<f32> for Point {
    type Output = Self;

    fn mul(self, other: f32) -> Point {
        Point(self.0 * other, self.1 * other)
    }
}

impl From<(f32, f32)> for Point {
    fn from(value: (f32, f32)) -> Point {
        Point(value.0, value.1)
    }
}

//----------------------------------------------------------------------------

/// Rectangle area within a grid of rectangular pixels. It consists of the four
/// values: top, left, width, and height. Both axes of the grid are aligned like
/// the coordinate system of a web page: `x` runs from left to right, `y` runs
/// from top to bottom.
///
/// # Examples
///
/// ```
/// # use rekee::hexagon::*;
/// let rect = Rect::new(1.0, 2.0, 4.0, 3.0);
/// assert_eq!(rect.left, 1.0);
/// assert_eq!(rect.top, 2.0);
/// assert_eq!(rect.right(), 5.0);
/// assert_eq!(rect.bottom(), 5.0);
/// ```
#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub struct Rect {
    pub left: f32,
    pub top: f32,
    pub width: f32,
    pub height: f32,
}

impl Rect {
    /// Creates a new rectangle from the `left`, `top`, `width` and `height`
    /// values. Values for `width` or `height` are allowed to be negative, the
    /// rectangle will be normalized in that case.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let rect = Rect::new(1.0, 2.0, 3.0, -4.0);
    /// assert_eq!(rect.left, 1.0);
    /// assert_eq!(rect.top, -2.0);
    /// assert_eq!(rect.right(), 4.0);
    /// assert_eq!(rect.bottom(), 2.0);
    /// ```
    pub fn new(left: f32, top: f32, width: f32, height: f32) -> Self {
        Rect { left, top, width, height }.normalized()
    }

    /// Right position of the rectangle.
    pub fn right(&self) -> f32 {
        self.left + self.width
    }

    /// Bottom position of the rectangle.
    pub fn bottom(&self) -> f32 {
        self.top + self.height
    }

    /// Center point of the rectangle.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let rect = Rect::new(0.0, 1.0, 3.0, 4.0);
    /// assert_eq!(rect.center(), Point(1.5, 3.0));
    /// ```
    pub fn center(&self) -> Point {
        let x = self.left + self.width / 2.0;
        let y = self.top + self.height / 2.0;
        Point(x, y)
    }

    /// Top-left corner of the rectangle.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let rect = Rect::new(0.0, 1.0, 3.0, 4.0);
    /// assert_eq!(rect.top_left(), Point(0.0, 1.0));
    /// assert_eq!(rect.left, 0.0);
    /// assert_eq!(rect.top, 1.0);
    /// ```
    pub fn top_left(&self) -> Point {
        Point(self.left, self.top)
    }

    /// Bottom-right corner of the rectangle.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let rect = Rect::new(0.0, 1.0, 3.0, 4.0);
    /// assert_eq!(rect.bottom_right(), Point(3.0, 5.0));
    /// assert_eq!(rect.right(), 3.0);
    /// assert_eq!(rect.bottom(), 5.0);
    /// ```
    pub fn bottom_right(&self) -> Point {
        Point(self.right(), self.bottom())
    }

    /// Calculates the union of two rectangles, the smallest rectangle that
    /// fully contains both source rectangles.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let a = Rect::new(1.0, 2.0, 3.0, 4.0);
    /// let b = Rect::new(-3.0, -4.0, 2.0, 1.0);
    /// let rect = a.union(&b);
    /// assert_eq!(rect.left, -3.0);
    /// assert_eq!(rect.top, -4.0);
    /// assert_eq!(rect.width, 7.0);
    /// assert_eq!(rect.height, 10.0);
    /// ```
    pub fn union(&self, other: &Rect) -> Rect {
        let a = self.normalized();
        let b = other.normalized();
        let left = a.left.min(b.left);
        let top = a.top.min(b.top);
        let width = a.right().max(b.right()) - left;
        let height = a.bottom().max(b.bottom()) - top;
        Rect { left, top, width, height }
    }

    /// Creates a new rectangle from the existing one by adding a padding border
    /// on all four sides.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let a = Rect::new(1.0, 2.0, 3.0, 4.0);
    /// let rect = a.with_padding(0.5);
    /// assert_eq!(rect.left, 0.5);
    /// assert_eq!(rect.top, 1.5);
    /// assert_eq!(rect.width, 4.0);
    /// assert_eq!(rect.height, 5.0);
    /// ```
    pub fn with_padding(&self, value: f32) -> Self {
        let left = self.left - value;
        let top = self.top - value;
        let width = self.width + value + value;
        let height = self.height + 2.0 * value;
        Rect { left, top, width, height }
    }

    fn normalized(&self) -> Rect {
        let mut left = self.left;
        let mut top = self.top;
        let width = if self.width >= 0.0 {
            self.width
        } else {
            left += self.width;
            -self.width
        };
        let height = if self.height >= 0.0 {
            self.height
        } else {
            top += self.height;
            -self.height
        };
        Rect { left, top, width, height }
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
    // base direction of the hex grid (in multiples of 60°)
    start_angle: FloatDirection,
}

#[allow(clippy::excessive_precision)]
const SQRT_3: f32 = 1.73205080756887729352744634150587237f32;

const LAYOUT_POINTY: Orientation = Orientation {
    f0: SQRT_3, f1: SQRT_3 / 2.0, f2: 0.0, f3: 3.0 / 2.0,
    b0: SQRT_3 / 3.0, b1: -1.0 / 3.0, b2: 0.0, b3: 2.0 / 3.0,
    start_angle: FloatDirection(1.5),
};

const LAYOUT_FLAT: Orientation = Orientation {
    f0: 0.0, f1: 3.0 / 2.0, f2: -SQRT_3, f3: -SQRT_3 / 2.0,
    b0: -1.0 / 3.0, b1: -SQRT_3 / 3.0, b2: 2.0 / 3.0, b3: 0.0,
    start_angle: FloatDirection(0.0),
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
        (f32::from(self.orientation.start_angle) - 1.5).abs() <= 0.1
    }

    /// Returns whether the hexagon orientation is "flat top" or not.
    pub fn is_flat(&self) -> bool {
        f32::from(self.orientation.start_angle).abs() <= 0.1
    }

    /// Creates a copy of this layout with new grid orientation parameters.
    pub fn with_orientation(&self, orientation: &Orientation) -> Self {
        Layout { orientation: orientation.clone(), size: self.size, origin: self.origin }
    }

    /// Creates a copy of this layout with new hexagon size parameters.
    pub fn with_size(&self, size: Point) -> Self {
        Layout { orientation: self.orientation.clone(), size, origin: self.origin }
    }

    /// Creates a copy of this layout with new grid origin point.
    pub fn with_origin(&self, origin: Point) -> Self {
        Layout { orientation: self.orientation.clone(), size: self.size, origin }
    }

    /// Calculates the corners of a hexagon with the given coordinate.
    pub fn hexagon_corners(&self, hex: Coordinate) -> [Point; 6] {
        let center = hex.to_pixel(self);
        let mut corners = [Point::default(); 6];
        for (i, corner) in corners.iter_mut().enumerate() {
            use std::f32::consts::PI;
            let angle = 2.0 * PI * (f32::from(self.orientation.start_angle) + f32::from(i as u8)) / 6.0;
            let offset_x = self.size.0 * angle.cos();
            let offset_y = self.size.1 * angle.sin();
            *corner = center + Point(offset_x, offset_y);
        }
        corners
    }

    /// Calculates the rectangular box of a hexagon with the given coordinate.
    pub fn hexagon_rect(&self, hex: Coordinate) -> Rect {
        let mut x_min = f32::NAN;
        let mut x_max = f32::NAN;
        let mut y_min = f32::NAN;
        let mut y_max = f32::NAN;
        for point in self.hexagon_corners(hex).iter() {
            x_min = x_min.min(point.x());
            x_max = x_max.max(point.x());
            y_min = y_min.min(point.y());
            y_max = y_max.max(point.y());
        }
        Rect { left: x_min, top: y_min, width: x_max - x_min, height: y_max - y_min }
    }

    /// Convert a hexagon grid direction into an angle in degrees.
    ///
    /// Applies the layout `start_angle` to the given direction. The retuned
    /// value is wrapped within a range of 0° to 360°.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::hexagon::*;
    /// let layout = Layout::new(Orientation::pointy(), Point(1.0, 1.0), Point(0.0, 0.0));
    ///
    /// let dir = Direction::A;
    /// assert_eq!(layout.direction_to_angle(dir), 90.0);
    ///
    /// let dir = Direction::B;
    /// assert_eq!(layout.direction_to_angle(dir), 150.0);
    ///
    /// let dir = FloatDirection(1.5);
    /// assert_eq!(layout.direction_to_angle(dir), 180.0);
    /// ```
    pub fn direction_to_angle<D>(&self, dir: D) -> f32
        where D: Into<FloatDirection>
    {
        let angle = dir.into().to_angle() + self.orientation.start_angle.to_angle();
        angle.rem_euclid(360.0)
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
        assert_eq!(pos + (1, 2).into(), Coordinate::new(1, 2));
        assert_eq!(pos + (-2, 3).into(), Coordinate::new(-2, 3));

        let pos = Coordinate::new(2, 1);
        assert_eq!(pos + (1, 2).into(), Coordinate::new(3, 3));
        assert_eq!(pos + (-2, 3).into(), Coordinate::new(0, 4));
    }

    #[test]
    fn coordinate_sub() {
        let pos = Coordinate::new(0, 0);
        assert_eq!(pos - (1, 2).into(), Coordinate::new(-1, -2));
        assert_eq!(pos - (-2, 3).into(), Coordinate::new(2, -3));

        let pos = Coordinate::new(2, 1);
        assert_eq!(pos - (1, 2).into(), Coordinate::new(1, -1));
        assert_eq!(pos - (-2, 3).into(), Coordinate::new(4, -2));
    }

    #[test]
    fn coordinate_to_pixel() {
        fn assert_approx_eq(left: Point, right: Point) {
            assert!(left.distance(right) < EPS, "left = {}, right = {}", left, right);
        }

        let layout = Layout::new(Orientation::pointy(), Point(10.0, 10.0), Point(0.0, 0.0));
        let pos = Coordinate::new(0, 0).to_pixel(&layout);
        assert_approx_eq(pos, Point(0.0, 0.0));
        let pos = Coordinate::new(0, 1).to_pixel(&layout);
        assert_approx_eq(pos, Point(8.660, 15.0));
        let pos = Coordinate::new(1, 0).to_pixel(&layout);
        assert_approx_eq(pos, Point(17.321, 0.0));
        let pos = Coordinate::new(2, -1).to_pixel(&layout);
        assert_approx_eq(pos, Point(25.981, -15.0));

        let layout = Layout::new(Orientation::pointy(), Point(20.0, -20.0), Point(0.0, 10.0));
        let pos = Coordinate::new(0, 0).to_pixel(&layout);
        assert_approx_eq(pos, Point(0.0, 10.0));
        let pos = Coordinate::new(0, 1).to_pixel(&layout);
        assert_approx_eq(pos, Point(17.321, -20.0));
        let pos = Coordinate::new(1, 0).to_pixel(&layout);
        assert_approx_eq(pos, Point(34.641, 10.0));
        let pos = Coordinate::new(2, -1).to_pixel(&layout);
        assert_approx_eq(pos, Point(51.962, 40.0));

        let layout = Layout::new(Orientation::flat(), Point(30.0, 20.0), Point(10.0, 0.0));
        let pos = Coordinate::new(0, 0).to_pixel(&layout);
        assert_approx_eq(pos, Point(10.0, 0.0));
        let pos = Coordinate::new(0, 1).to_pixel(&layout);
        assert_approx_eq(pos, Point(55.0, -17.321));
        let pos = Coordinate::new(1, 0).to_pixel(&layout);
        assert_approx_eq(pos, Point(10.0, -34.641));
        let pos = Coordinate::new(2, -1).to_pixel(&layout);
        assert_approx_eq(pos, Point(-35.0, -51.962));
    }

    #[test]
    fn coordinate_from_pixel_rounded() {
        let layout = Layout::new(Orientation::pointy(), Point(10.0, 10.0), Point(0.0, 0.0));
        let pos = Coordinate::from_pixel_rounded(&layout, Point(0.0, 0.0));
        assert_eq!(pos, Coordinate::new(0, 0));
        let pos = Coordinate::from_pixel_rounded(&layout, Point(5.0, -5.0));
        assert_eq!(pos, Coordinate::new(0, 0));
        let pos = Coordinate::from_pixel_rounded(&layout, Point(8.0, 15.0));
        assert_eq!(pos, Coordinate::new(0, 1));
        let pos = Coordinate::from_pixel_rounded(&layout, Point(18.0, 1.0));
        assert_eq!(pos, Coordinate::new(1, 0));
        let pos = Coordinate::from_pixel_rounded(&layout, Point(26.0, -16.0));
        assert_eq!(pos, Coordinate::new(2, -1));

        let layout = Layout::new(Orientation::pointy(), Point(20.0, -20.0), Point(0.0, 10.0));
        let pos = Coordinate::from_pixel_rounded(&layout, Point(0.0, 9.0));
        assert_eq!(pos, Coordinate::new(0, 0));
        let pos = Coordinate::from_pixel_rounded(&layout, Point(18.0, -21.0));
        assert_eq!(pos, Coordinate::new(0, 1));
        let pos = Coordinate::from_pixel_rounded(&layout, Point(33.0, 11.0));
        assert_eq!(pos, Coordinate::new(1, 0));
        let pos = Coordinate::from_pixel_rounded(&layout, Point(52.0, 41.0));
        assert_eq!(pos, Coordinate::new(2, -1));

        let layout = Layout::new(Orientation::flat(), Point(30.0, 20.0), Point(10.0, 0.0));
        let pos = Coordinate::from_pixel_rounded(&layout, Point(9.1, 0.5));
        assert_eq!(pos, Coordinate::new(0, 0));
        let pos = Coordinate::from_pixel_rounded(&layout, Point(53.4, -18.2));
        assert_eq!(pos, Coordinate::new(0, 1));
        let pos = Coordinate::from_pixel_rounded(&layout, Point(12.6, -32.3));
        assert_eq!(pos, Coordinate::new(1, 0));
        let pos = Coordinate::from_pixel_rounded(&layout, Point(-36.7, -53.8));
        assert_eq!(pos, Coordinate::new(2, -1));
    }

    #[test]
    fn coordinate_serde() {
        let pos = Coordinate::new(2, -1);
        let text = serde_json::to_string(&pos).unwrap();
        assert_eq!(text, r#"{"q":2,"r":-1}"#);

        let text = r#"{"q": 1, "r": -2}"#;
        let pos: Coordinate = serde_json::from_str(text).unwrap();
        assert_eq!(pos, Coordinate::new(1, -2));
    }

    #[test]
    fn float_coordinate_rotate() {
        fn assert_approx_eq(left: FloatCoordinate, right: FloatCoordinate) {
            assert!(left.distance(right) < EPS, "left = {}, right = {}", left, right);
        }

        let pos = FloatCoordinate::new(0.0, 0.0).rotate(0.0);
        assert_approx_eq(pos, FloatCoordinate::new(0.0, 0.0));
        let pos = FloatCoordinate::new(0.0, 1.5).rotate(0.0);
        assert_approx_eq(pos, FloatCoordinate::new(0.0, 1.5));
        let pos = FloatCoordinate::new(1.5, 0.0).rotate(0.0);
        assert_approx_eq(pos, FloatCoordinate::new(1.5, 0.0));
        let pos = FloatCoordinate::new(2.0, -1.0).rotate(0.0);
        assert_approx_eq(pos, FloatCoordinate::new(2.0, -1.0));

        let pos = FloatCoordinate::new(2.0, -1.0).rotate(-1.0);
        assert_approx_eq(pos, FloatCoordinate::new(1.0, -2.0));
        let pos = FloatCoordinate::new(2.0, -1.0).rotate(Direction::F);
        assert_approx_eq(pos, FloatCoordinate::new(1.0, -2.0));
        let pos = FloatCoordinate::new(2.0, -1.0).rotate(3.5);
        assert_approx_eq(pos, FloatCoordinate::new(-SQRT_3, 0.0));
        let pos = FloatCoordinate::new(2.0, -1.0).rotate(-2.5);
        assert_approx_eq(pos, FloatCoordinate::new(-SQRT_3, 0.0));
    }

    #[test]
    fn float_coordinate_to_pixel() {
        fn assert_approx_eq(left: Point, right: Point) {
            assert!(left.distance(right) < EPS, "left = {}, right = {}", left, right);
        }

        let layout = Layout::new(Orientation::pointy(), Point(10.0, 10.0), Point(0.0, 0.0));
        let pos = FloatCoordinate::new(0.0, 0.0).to_pixel(&layout);
        assert_approx_eq(pos, Point(0.0, 0.0));
        let pos = FloatCoordinate::new(0.0, 1.5).to_pixel(&layout);
        assert_approx_eq(pos, Point(12.990, 22.5));
        let pos = FloatCoordinate::new(1.5, 0.0).to_pixel(&layout);
        assert_approx_eq(pos, Point(25.981, 0.0));
        let pos = FloatCoordinate::new(2.0, -1.0).to_pixel(&layout);
        assert_approx_eq(pos, Point(25.981, -15.0));

        let layout = Layout::new(Orientation::pointy(), Point(20.0, -20.0), Point(0.0, 10.0));
        let pos = FloatCoordinate::new(0.0, 0.0).to_pixel(&layout);
        assert_approx_eq(pos, Point(0.0, 10.0));
        let pos = FloatCoordinate::new(0.0, 1.5).to_pixel(&layout);
        assert_approx_eq(pos, Point(25.981, -35.0));
        let pos = FloatCoordinate::new(1.5, 0.0).to_pixel(&layout);
        assert_approx_eq(pos, Point(51.962, 10.0));
        let pos = FloatCoordinate::new(2.0, -1.0).to_pixel(&layout);
        assert_approx_eq(pos, Point(51.962, 40.0));

        let layout = Layout::new(Orientation::flat(), Point(30.0, 20.0), Point(10.0, 0.0));
        let pos = FloatCoordinate::new(0.0, 0.0).to_pixel(&layout);
        assert_approx_eq(pos, Point(10.0, 0.0));
        let pos = FloatCoordinate::new(0.0, 1.5).to_pixel(&layout);
        assert_approx_eq(pos, Point(77.5, -25.981));
        let pos = FloatCoordinate::new(1.5, 0.0).to_pixel(&layout);
        assert_approx_eq(pos, Point(10.0, -51.962));
        let pos = FloatCoordinate::new(2.0, -1.0).to_pixel(&layout);
        assert_approx_eq(pos, Point(-35.0, -51.962));
    }

    #[test]
    fn float_coordinate_from_pixel() {
        fn assert_approx_eq(left: FloatCoordinate, right: FloatCoordinate) {
            assert!(left.distance(right) < EPS, "left = {}, right = {}", left, right);
        }

        let layout = Layout::new(Orientation::pointy(), Point(10.0, 10.0), Point(0.0, 0.0));
        let pos = FloatCoordinate::from_pixel(&layout, Point(0.0, 0.0));
        assert_approx_eq(pos, FloatCoordinate::new(0.0, 0.0));
        let pos = FloatCoordinate::from_pixel(&layout, Point(5.0, -5.0));
        assert_approx_eq(pos, FloatCoordinate::new(0.455, -0.333));
        let pos = FloatCoordinate::from_pixel(&layout, Point(12.990, 22.5));
        assert_approx_eq(pos, FloatCoordinate::new(0.0, 1.5));
        let pos = FloatCoordinate::from_pixel(&layout, Point(25.981, 0.0));
        assert_approx_eq(pos, FloatCoordinate::new(1.5, 0.0));
        let pos = FloatCoordinate::from_pixel(&layout, Point(25.981, -15.0));
        assert_approx_eq(pos, FloatCoordinate::new(2.0, -1.0));

        let layout = Layout::new(Orientation::pointy(), Point(20.0, -20.0), Point(0.0, 10.0));
        let pos = FloatCoordinate::from_pixel(&layout, Point(0.0, 10.0));
        assert_approx_eq(pos, FloatCoordinate::new(0.0, 0.0));
        let pos = FloatCoordinate::from_pixel(&layout, Point(25.981, -35.0));
        assert_approx_eq(pos, FloatCoordinate::new(0.0, 1.5));
        let pos = FloatCoordinate::from_pixel(&layout, Point(51.962, 10.0));
        assert_approx_eq(pos, FloatCoordinate::new(1.5, 0.0));
        let pos = FloatCoordinate::from_pixel(&layout, Point(51.962, 40.0));
        assert_approx_eq(pos, FloatCoordinate::new(2.0, -1.0));

        let layout = Layout::new(Orientation::flat(), Point(30.0, 20.0), Point(10.0, 0.0));
        let pos = FloatCoordinate::from_pixel(&layout, Point(10.0, 0.0));
        assert_approx_eq(pos, FloatCoordinate::new(0.0, 0.0));
        let pos = FloatCoordinate::from_pixel(&layout, Point(77.5, -25.981));
        assert_approx_eq(pos, FloatCoordinate::new(0.0, 1.5));
        let pos = FloatCoordinate::from_pixel(&layout, Point(10.0, -51.962));
        assert_approx_eq(pos, FloatCoordinate::new(1.5, 0.0));
        let pos = FloatCoordinate::from_pixel(&layout, Point(-35.0, -51.962));
        assert_approx_eq(pos, FloatCoordinate::new(2.0, -1.0));
    }

    #[test]
    fn float_coordinate_serde() {
        let pos = FloatCoordinate::new(2.5, -1.25);
        let text = serde_json::to_string(&pos).unwrap();
        assert_eq!(text, r#"{"q":2.5,"r":-1.25}"#);

        let text = r#"{"q": 1.25, "r": -2.5}"#;
        let pos: FloatCoordinate = serde_json::from_str(text).unwrap();
        assert_eq!(pos, FloatCoordinate::new(1.25, -2.5));
    }

    #[test]
    fn direction_add() {
        let dir = Direction::A;
        assert_eq!(dir + 1.into(), Direction::B);
        assert_eq!(dir + 3.into(), Direction::D);

        let dir = Direction::E;
        assert_eq!(dir + 1.into(), Direction::F);
        assert_eq!(dir + 3.into(), Direction::B);
    }

    #[test]
    fn direction_sub() {
        let dir = Direction::A;
        assert_eq!(dir - 1.into(), Direction::F);
        assert_eq!(dir - 3.into(), Direction::D);

        let dir = Direction::E;
        assert_eq!(dir - 1.into(), Direction::D);
        assert_eq!(dir - 3.into(), Direction::B);
    }

    #[test]
    fn direction_opposite() {
        let dir = Direction::B;
        assert_eq!(dir.opposite(), Direction::E);

        let dir = Direction::F;
        assert_eq!(dir.opposite(), Direction::C);
    }

    #[test]
    fn direction_to_angle() {
        fn assert_approx_eq(left: f32, right: f32) {
            assert!((left - right).abs() < EPS, "left = {}, right = {}", left, right);
        }

        assert_approx_eq(Direction::A.to_angle(), 0.0);
        assert_approx_eq(Direction::B.to_angle(), 60.0);
        assert_approx_eq(Direction::C.to_angle(), 120.0);
        assert_approx_eq(Direction::D.to_angle(), 180.0);
        assert_approx_eq(Direction::E.to_angle(), 240.0);
        assert_approx_eq(Direction::F.to_angle(), 300.0);

        let layout = Layout::new(Orientation::pointy(), Point(10.0, 10.0), Point(0.0, 0.0));
        assert_approx_eq(layout.direction_to_angle(Direction::A), 90.0);
        assert_approx_eq(layout.direction_to_angle(Direction::B), 150.0);
        assert_approx_eq(layout.direction_to_angle(Direction::C), 210.0);
        assert_approx_eq(layout.direction_to_angle(Direction::D), 270.0);
        assert_approx_eq(layout.direction_to_angle(Direction::E), 330.0);
        assert_approx_eq(layout.direction_to_angle(Direction::F), 30.0);

        let layout = Layout::new(Orientation::pointy(), Point(20.0, -20.0), Point(0.0, 10.0));
        assert_approx_eq(layout.direction_to_angle(Direction::from(-1)), 30.0);
        assert_approx_eq(layout.direction_to_angle(Direction::from(0)), 90.0);
        assert_approx_eq(layout.direction_to_angle(Direction::from(1)), 150.0);
        assert_approx_eq(layout.direction_to_angle(Direction::from(2)), 210.0);
        assert_approx_eq(layout.direction_to_angle(Direction::from(3)), 270.0);
        assert_approx_eq(layout.direction_to_angle(Direction::from(4)), 330.0);
        assert_approx_eq(layout.direction_to_angle(Direction::from(5)), 30.0);
        assert_approx_eq(layout.direction_to_angle(Direction::from(6)), 90.0);

        let layout = Layout::new(Orientation::flat(), Point(30.0, 20.0), Point(10.0, 0.0));
        assert_approx_eq(layout.direction_to_angle(Direction::A), 0.0);
        assert_approx_eq(layout.direction_to_angle(Direction::B), 60.0);
        assert_approx_eq(layout.direction_to_angle(Direction::C), 120.0);
        assert_approx_eq(layout.direction_to_angle(Direction::D), 180.0);
        assert_approx_eq(layout.direction_to_angle(Direction::E), 240.0);
        assert_approx_eq(layout.direction_to_angle(Direction::F), 300.0);
    }

    #[test]
    fn direction_serde() {
        let dir = Direction::A;
        let text = serde_json::to_string(&dir).unwrap();
        assert_eq!(text, r#"0"#);

        let text = r#"2"#;
        let dir: Direction = serde_json::from_str(text).unwrap();
        assert_eq!(dir, Direction::C);

        let text = r#"6"#;
        let dir: Direction = serde_json::from_str(text).unwrap();
        assert_eq!(dir, Direction::A);

        let text = r#"-1"#;
        let result: Result<Direction, _> = serde_json::from_str(text);
        assert!(result.is_err());

        let text = r#"hello"#;
        let result: Result<Direction, _> = serde_json::from_str(text);
        assert!(result.is_err());
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
        assert_eq!(layout.is_pointy(), true);
        assert_eq!(layout.is_flat(), false);
        let corners = layout.hexagon_corners((0, 0).into());
        assert_eq!(points_to_string(&corners), "(-0.0, 10.0), (-8.7, 5.0), (-8.7, -5.0), (0.0, -10.0), (8.7, -5.0), (8.7, 5.0)");
        let corners = layout.hexagon_corners((0, 1).into());
        assert_eq!(points_to_string(&corners), "(8.7, 25.0), (-0.0, 20.0), (0.0, 10.0), (8.7, 5.0), (17.3, 10.0), (17.3, 20.0)");
        let corners = layout.hexagon_corners((1, 0).into());
        assert_eq!(points_to_string(&corners), "(17.3, 10.0), (8.7, 5.0), (8.7, -5.0), (17.3, -10.0), (26.0, -5.0), (26.0, 5.0)");
        let corners = layout.hexagon_corners((2, -1).into());
        assert_eq!(points_to_string(&corners), "(26.0, -5.0), (17.3, -10.0), (17.3, -20.0), (26.0, -25.0), (34.6, -20.0), (34.6, -10.0)");

        let layout = Layout::new(Orientation::pointy(), Point(20.0, -20.0), Point(0.0, 10.0));
        assert_eq!(layout.is_pointy(), true);
        assert_eq!(layout.is_flat(), false);
        let corners = layout.hexagon_corners((0, 0).into());
        assert_eq!(points_to_string(&corners), "(-0.0, -10.0), (-17.3, 0.0), (-17.3, 20.0), (0.0, 30.0), (17.3, 20.0), (17.3, -0.0)");
        let corners = layout.hexagon_corners((0, 1).into());
        assert_eq!(points_to_string(&corners), "(17.3, -40.0), (-0.0, -30.0), (0.0, -10.0), (17.3, 0.0), (34.6, -10.0), (34.6, -30.0)");
        let corners = layout.hexagon_corners((1, 0).into());
        assert_eq!(points_to_string(&corners), "(34.6, -10.0), (17.3, 0.0), (17.3, 20.0), (34.6, 30.0), (52.0, 20.0), (52.0, -0.0)");
        let corners = layout.hexagon_corners((2, -1).into());
        assert_eq!(points_to_string(&corners), "(52.0, 20.0), (34.6, 30.0), (34.6, 50.0), (52.0, 60.0), (69.3, 50.0), (69.3, 30.0)");

        let layout = Layout::new(Orientation::flat(), Point(30.0, 20.0), Point(10.0, 0.0));
        assert_eq!(layout.is_pointy(), false);
        assert_eq!(layout.is_flat(), true);
        let corners = layout.hexagon_corners((0, 0).into());
        assert_eq!(points_to_string(&corners), "(40.0, 0.0), (25.0, 17.3), (-5.0, 17.3), (-20.0, -0.0), (-5.0, -17.3), (25.0, -17.3)");
        let corners = layout.hexagon_corners((0, 1).into());
        assert_eq!(points_to_string(&corners), "(85.0, -17.3), (70.0, 0.0), (40.0, 0.0), (25.0, -17.3), (40.0, -34.6), (70.0, -34.6)");
        let corners = layout.hexagon_corners((1, 0).into());
        assert_eq!(points_to_string(&corners), "(40.0, -34.6), (25.0, -17.3), (-5.0, -17.3), (-20.0, -34.6), (-5.0, -52.0), (25.0, -52.0)");
        let corners = layout.hexagon_corners((2, -1).into());
        assert_eq!(points_to_string(&corners), "(-5.0, -52.0), (-20.0, -34.6), (-50.0, -34.6), (-65.0, -52.0), (-50.0, -69.3), (-20.0, -69.3)");
    }

    #[test]
    fn layout_hexagon_rect() {
        let rect_to_string = |rect: &Rect| -> String {
            format!("({:.1}, {:.1}) {:.1}x{:.1}", rect.left, rect.top, rect.width, rect.height)
        };

        let layout = Layout::new(Orientation::pointy(), Point(10.0, 10.0), Point(0.0, 0.0));
        let rect = layout.hexagon_rect((0, 0).into());
        assert_eq!(rect_to_string(&rect), "(-8.7, -10.0) 17.3x20.0");
        let mut total = rect;
        let rect = layout.hexagon_rect((0, 1).into());
        assert_eq!(rect_to_string(&rect), "(-0.0, 5.0) 17.3x20.0");
        total = total.union(&rect);
        let rect = layout.hexagon_rect((1, 0).into());
        assert_eq!(rect_to_string(&rect), "(8.7, -10.0) 17.3x20.0");
        total = total.union(&rect);
        let rect = layout.hexagon_rect((2, -1).into());
        assert_eq!(rect_to_string(&rect), "(17.3, -25.0) 17.3x20.0");
        total = total.union(&rect);
        assert_eq!(rect_to_string(&total), "(-8.7, -25.0) 43.3x50.0");

        let layout = Layout::new(Orientation::pointy(), Point(20.0, -20.0), Point(0.0, 10.0));
        let rect = layout.hexagon_rect((0, 0).into());
        assert_eq!(rect_to_string(&rect), "(-17.3, -10.0) 34.6x40.0");
        let mut total = rect;
        let rect = layout.hexagon_rect((0, 1).into());
        assert_eq!(rect_to_string(&rect), "(-0.0, -40.0) 34.6x40.0");
        total = total.union(&rect);
        let rect = layout.hexagon_rect((1, 0).into());
        assert_eq!(rect_to_string(&rect), "(17.3, -10.0) 34.6x40.0");
        total = total.union(&rect);
        let rect = layout.hexagon_rect((2, -1).into());
        assert_eq!(rect_to_string(&rect), "(34.6, 20.0) 34.6x40.0");
        total = total.union(&rect);
        assert_eq!(rect_to_string(&total), "(-17.3, -40.0) 86.6x100.0");

        let layout = Layout::new(Orientation::flat(), Point(30.0, 20.0), Point(10.0, 0.0));
        let rect = layout.hexagon_rect((0, 0).into());
        assert_eq!(rect_to_string(&rect), "(-20.0, -17.3) 60.0x34.6");
        let mut total = rect;
        let rect = layout.hexagon_rect((0, 1).into());
        assert_eq!(rect_to_string(&rect), "(25.0, -34.6) 60.0x34.6");
        total = total.union(&rect);
        let rect = layout.hexagon_rect((1, 0).into());
        assert_eq!(rect_to_string(&rect), "(-20.0, -52.0) 60.0x34.6");
        total = total.union(&rect);
        let rect = layout.hexagon_rect((2, -1).into());
        assert_eq!(rect_to_string(&rect), "(-65.0, -69.3) 60.0x34.6");
        total = total.union(&rect);
        assert_eq!(rect_to_string(&total), "(-65.0, -69.3) 150.0x86.6");
    }
}

//----------------------------------------------------------------------------
