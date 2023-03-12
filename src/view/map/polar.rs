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

#[derive(Clone, Copy, Debug, PartialEq)]
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

    pub fn from_coordinate( pos: FloatCoordinate ) -> Self {
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

#[cfg(test)]
impl approx::AbsDiffEq for PolarCoordinate {
    type Epsilon = f32;

    fn default_epsilon() -> f32 {
        tests::EPSILON
    }

    fn abs_diff_eq(&self, other: &Self, epsilon: f32) -> bool {
        f32::abs_diff_eq(&self.distance, &other.distance, epsilon) &&
        f32::abs_diff_eq(&self.angle, &other.angle, epsilon)
    }
}

//----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use approx::assert_abs_diff_eq;
    use crate::hexagon::FloatCoordinate;
    use super::*;

    pub const EPSILON: f32 = 1e-5;

    #[test]
    fn polar_from_coordinate() {
        let polar = PolarCoordinate::from_coordinate((1.0, 0.0).into());
        assert_abs_diff_eq!(polar, PolarCoordinate::new(1.0, 0.0));

        let polar = PolarCoordinate::from_coordinate((0.0, 1.0).into());
        assert_abs_diff_eq!(polar, PolarCoordinate::new(1.0, 60.0));

        let polar = PolarCoordinate::from_coordinate((2.0, -1.0).into());
        assert_abs_diff_eq!(polar, PolarCoordinate::new(SQRT_3, -30.0));

        let polar = PolarCoordinate::from_coordinate((-2.0, 1.5).into());
        assert_abs_diff_eq!(polar, PolarCoordinate::new(1.802776, 133.89789));
    }

    #[test]
    fn polar_to_coordinate() {
        let pos = PolarCoordinate::new(1.0, 0.0).to_coordinate();
        assert_abs_diff_eq!(pos, FloatCoordinate::new(1.0, 0.0));

        let pos = PolarCoordinate::new(1.0, 60.0).to_coordinate();
        assert_abs_diff_eq!(pos, FloatCoordinate::new(0.0, 1.0));

        let pos = PolarCoordinate::new(SQRT_3, -30.0).to_coordinate();
        assert_abs_diff_eq!(pos, FloatCoordinate::new(2.0, -1.0));

        let pos = PolarCoordinate::new(1.802776, 133.89789).to_coordinate();
        assert_abs_diff_eq!(pos, FloatCoordinate::new(-2.0, 1.5));
    }
}

//----------------------------------------------------------------------------
