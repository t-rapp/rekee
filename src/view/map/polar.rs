//----------------------------------------------------------------------------
//! Polar coordinates and their mapping to/from hexagon coordinates.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use std::fmt;

use crate::hexagon::FloatCoordinate;

//----------------------------------------------------------------------------

#[derive(Clone, Copy, Debug)]
pub struct PolarCoordinate {
    distance: f32,
    angle: f32,
}

#[allow(clippy::excessive_precision)]
const SQRT_3: f32 = 1.73205080756887729352744634150587237_f32;

/// Coordinate in polar form.
///
/// Used by token input controls for positioning using distance and angle within
/// a hexagonal grid.
impl PolarCoordinate {
    pub const fn new(distance: f32, angle: f32) -> Self {
        PolarCoordinate { distance, angle }
    }

    pub fn distance(&self) -> f32 {
        self.distance
    }

    pub fn angle(&self) -> f32 {
        self.angle
    }

    pub fn from_coordinate<C>( pos: C ) -> Self
        where C: Into<FloatCoordinate>
    {
        let pos = pos.into();
        let distance = (pos.q() * pos.q() + pos.r() * pos.r() + pos.q() * pos.r()).sqrt();
        let phi = f32::atan2(pos.r() * SQRT_3 / 2.0, pos.q() + pos.r() / 2.0);
        let angle = phi.to_degrees();
        PolarCoordinate { distance, angle }
    }

    pub fn to_coordinate(self) -> FloatCoordinate {
        let phi = self.angle.to_radians();
        let q = self.distance * phi.cos() - self.distance * phi.sin() / SQRT_3;
        let r = self.distance * phi.sin() * 2.0 / SQRT_3;
        FloatCoordinate::new(q, r)
    }
}

impl fmt::Display for PolarCoordinate {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "({}, {})", self.distance, self.angle)
    }
}

//----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::hexagon::FloatCoordinate;
    use super::*;

    const EPS: f32 = 0.001;

    #[test]
    fn polar_from_coordinate() {
        fn assert_approx_eq(left: PolarCoordinate, right: PolarCoordinate) {
            assert!((left.distance - right.distance).abs() < EPS && (left.angle - right.angle).abs() < EPS, "left = {}, right = {}", left, right);
        }

        let polar = PolarCoordinate::from_coordinate((1.0, 0.0));
        assert_approx_eq(PolarCoordinate::new(1.0, 0.0), polar);

        let polar = PolarCoordinate::from_coordinate((0.0, 1.0));
        assert_approx_eq(PolarCoordinate::new(1.0, 60.0), polar);

        let polar = PolarCoordinate::from_coordinate((2.0, -1.0));
        assert_approx_eq(PolarCoordinate::new(SQRT_3, -30.0), polar);

        let polar = PolarCoordinate::from_coordinate((-2.0, 1.5));
        assert_approx_eq(PolarCoordinate::new(1.803, 133.898), polar);
    }

    #[test]
    fn polar_to_coordinate() {
        fn assert_approx_eq(left: FloatCoordinate, right: FloatCoordinate) {
            assert!((left.q() - right.q()).abs() < EPS && (left.r() - right.r()).abs() < EPS, "left = {}, right = {}", left, right);
        }

        let pos = PolarCoordinate::new(1.0, 0.0).to_coordinate();
        assert_approx_eq(FloatCoordinate::new(1.0, 0.0), pos);

        let pos = PolarCoordinate::new(1.0, 60.0).to_coordinate();
        assert_approx_eq(FloatCoordinate::new(0.0, 1.0), pos);

        let pos = PolarCoordinate::new(SQRT_3, -30.0).to_coordinate();
        assert_approx_eq(FloatCoordinate::new(2.0, -1.0), pos);

        let pos = PolarCoordinate::new(1.803, 133.898).to_coordinate();
        assert_approx_eq(FloatCoordinate::new(-2.0, 1.5), pos);
    }
}

//----------------------------------------------------------------------------
