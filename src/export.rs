//----------------------------------------------------------------------------
//! Utilitiy functions for map image export.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use std::fmt;
use std::str::FromStr;

use serde::{Serialize, Deserialize};

//----------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "kebab-case", try_from = "&str")]
pub enum ExportScale {
    Small,
    Medium,
    Large,
    ExtraLarge,
}

impl ExportScale {
    pub const fn title_height(&self) -> i32 {
        match self {
            ExportScale::Small => 20,
            ExportScale::Medium => 24,
            ExportScale::Large => 30,
            ExportScale::ExtraLarge => 40,
        }
    }

    pub const fn author_height(&self) -> i32 {
        match self {
            ExportScale::Small => 14,
            ExportScale::Medium => 16,
            ExportScale::Large => 21,
            ExportScale::ExtraLarge => 28,
        }
    }

    pub const fn tile_label_height(&self) -> i32 {
        match self {
            ExportScale::Small => 14,
            ExportScale::Medium => 16,
            ExportScale::Large => 20,
            ExportScale::ExtraLarge => 26,
        }
    }
}

impl Default for ExportScale {
    fn default() -> Self {
        ExportScale::Medium
    }
}

impl fmt::Display for ExportScale {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExportScale::Small =>
                write!(fmt, "Small")?,
            ExportScale::Medium =>
                write!(fmt, "Medium")?,
            ExportScale::Large =>
                write!(fmt, "Large")?,
            ExportScale::ExtraLarge =>
                write!(fmt, "Extra Large")?,
        }
        Ok(())
    }
}

impl FromStr for ExportScale {
    type Err = ParseExportScaleError;

    fn from_str(val: &str) -> std::result::Result<Self, Self::Err> {
        // match strings from both trait implementations, std::fmt::Display and serde::Serialize
        let mut s = val.replace(char::is_whitespace, "-");
        s.make_ascii_lowercase();

        match s.as_ref() {
            "small" =>
                Ok(ExportScale::Small),
            "medium" =>
                Ok(ExportScale::Medium),
            "large" =>
                Ok(ExportScale::Large),
            "extra-large" =>
                Ok(ExportScale::ExtraLarge),
            _ =>
                Err(ParseExportScaleError::Unknown(val.to_string())),
        }
    }
}

impl From<ExportScale> for f32 {
    fn from(val: ExportScale) -> f32 {
        match val {
            ExportScale::Small =>
                1.0,
            ExportScale::Medium =>
                1.2,
            ExportScale::Large =>
                1.5,
            ExportScale::ExtraLarge =>
                2.0,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseExportScaleError {
    Unknown(String),
}

impl fmt::Display for ParseExportScaleError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseExportScaleError::Unknown(val) =>
                write!(fmt, "Unknown export scale value \"{}\"", val),
        }
    }
}

impl std::convert::TryFrom<&str> for ExportScale {
    type Error = ParseExportScaleError;

    fn try_from(value: &str) -> std::result::Result<Self, Self::Error> {
        ExportScale::from_str(value)
    }
}

//----------------------------------------------------------------------------

pub mod util {
    use crate::hexagon::{Coordinate, Layout, Orientation, Point};
    use crate::token::TokenId;

    pub fn tile_image_size(layout: &Layout) -> Point {
        // use flat layout orientation as we confine us to the image size without rotation applied yet
        let rect = layout.with_orientation(Orientation::flat())
            .hexagon_rect(Coordinate::new(0, 0));
        Point(rect.width.round(), rect.height.round())
    }

    pub fn tile_image_center(layout: &Layout) -> Point {
        let size = tile_image_size(layout);
        Point(0.5 * size.x(), 0.5 * size.y())
    }

    pub fn token_image_size(layout: &Layout, token_id: TokenId) -> Point {
        match token_id {
            TokenId::Chicane(_) | TokenId::ChicaneWithLimit(_) | TokenId::Jump(_) | TokenId::Water(_) =>
                Point(0.500 * layout.size().x(), 0.350 * layout.size().y()),
            TokenId::MudSpray | TokenId::ClimbAscent | TokenId::ClimbDescent | TokenId::Cloud | TokenId::Oxygen(_) =>
                Point(0.315 * layout.size().x(), 0.315 * layout.size().y()),
            TokenId::Finish | TokenId::JokerEntrance | TokenId::JokerExit =>
                Point(1.200 * layout.size().x(), 0.585 * layout.size().y()),
        }
    }

    pub fn token_image_center(layout: &Layout, token_id: TokenId) -> Point {
        let size = token_image_size(layout, token_id);
        match token_id {
            TokenId::Chicane(_) | TokenId::ChicaneWithLimit(_) | TokenId::Jump(_) | TokenId::Water(_) =>
                Point(0.5 * size.x(), 0.5 * size.y()),
            TokenId::MudSpray | TokenId::ClimbAscent | TokenId::ClimbDescent | TokenId::Cloud | TokenId::Oxygen(_) =>
                Point(0.5 * size.x(), 0.5 * size.y()),
            TokenId::Finish | TokenId::JokerEntrance | TokenId::JokerExit =>
                Point(0.5 * size.x(), size.y()),
        }
    }
}

//----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use approx::assert_abs_diff_eq;

    use crate::hexagon::{Layout, Orientation, Point};
    use crate::token::TokenId;
    use super::*;

    #[test]
    fn tile_image_size_and_center() {
        let layout = Layout::new(Orientation::pointy(), Point(60.0, 60.0), Point(0.0, 0.0));
        let size = util::tile_image_size(&layout);
        assert_abs_diff_eq!(size, Point(120.0, 104.0));
        let center = util::tile_image_center(&layout);
        assert_abs_diff_eq!(center, Point(60.0, 52.0));

        let layout = Layout::new(Orientation::flat(), Point(60.0, 60.0), Point(0.0, 0.0));
        let size = util::tile_image_size(&layout);
        assert_abs_diff_eq!(size, Point(120.0, 104.0));
        let center = util::tile_image_center(&layout);
        assert_abs_diff_eq!(center, Point(60.0, 52.0));
    }

    #[test]
    fn token_image_size_and_center() {
        use crate::tile::Terrain;

        let layout = Layout::new(Orientation::pointy(), Point(60.0, 60.0), Point(0.0, 0.0));

        let token_id = TokenId::Chicane(Terrain::Asphalt);
        let size = util::token_image_size(&layout, token_id);
        assert_abs_diff_eq!(size, Point(30.0, 21.0));
        let center = util::token_image_center(&layout, token_id);
        assert_abs_diff_eq!(center, Point(15.0, 10.5));

        let token_id = TokenId::ClimbAscent;
        let size = util::token_image_size(&layout, token_id);
        assert_abs_diff_eq!(size, Point(18.9, 18.9));
        let center = util::token_image_center(&layout, token_id);
        assert_abs_diff_eq!(center, Point(9.45, 9.45));

        let token_id = TokenId::JokerEntrance;
        let size = util::token_image_size(&layout, token_id);
        assert_abs_diff_eq!(size, Point(72.0, 35.1));
        let center = util::token_image_center(&layout, token_id);
        assert_abs_diff_eq!(center, Point(36.0, 35.1));

        let layout = Layout::new(Orientation::flat(), Point(60.0, 60.0), Point(0.0, 0.0));

        let token_id = TokenId::Chicane(Terrain::Asphalt);
        let size = util::token_image_size(&layout, token_id);
        assert_abs_diff_eq!(size, Point(30.0, 21.0));
        let center = util::token_image_center(&layout, token_id);
        assert_abs_diff_eq!(center, Point(15.0, 10.5));

        let token_id = TokenId::ClimbAscent;
        let size = util::token_image_size(&layout, token_id);
        assert_abs_diff_eq!(size, Point(18.9, 18.9));
        let center = util::token_image_center(&layout, token_id);
        assert_abs_diff_eq!(center, Point(9.45, 9.45));

        let token_id = TokenId::JokerExit;
        let size = util::token_image_size(&layout, token_id);
        assert_abs_diff_eq!(size, Point(72.0, 35.1));
        let center = util::token_image_center(&layout, token_id);
        assert_abs_diff_eq!(center, Point(36.0, 35.1));
    }
}

//----------------------------------------------------------------------------
