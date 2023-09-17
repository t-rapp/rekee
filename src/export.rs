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

use crate::edition::Series;
use crate::hexagon::Point;

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

    pub const fn tile_size(&self) -> Point {
        match self {
            ExportScale::Small => Point(60.0, 60.0),
            ExportScale::Medium => Point(72.0, 72.0),
            ExportScale::Large => Point(90.0, 90.0),
            ExportScale::ExtraLarge => Point(120.0, 120.0),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Serialize, Deserialize)]
#[serde(transparent)]
pub struct ExportColorScheme {
    series: Series,
}

impl ExportColorScheme {
    pub fn new(series: Series) -> Self {
        ExportColorScheme { series }
    }

    pub fn background_color(&self) -> &'static str {
        "hsl(0, 0%, 100%)"
    }

    pub fn map_title_color(&self) -> &'static str {
        "hsl(12, 71%, 43%)"
    }

    pub fn map_author_color(&self) -> &'static str {
        "hsl(12, 56%, 67%)"
    }

    pub fn map_preposition_color(&self) -> &'static str {
        match self.series {
            Series::Gt => "hsl(206, 59%, 65%)",
            Series::Dirt => "hsl(93, 27%, 63%)",
        }
    }

    pub fn map_border_color(&self) -> &'static str {
        match self.series {
            Series::Gt => "hsl(206, 59%, 55%)",
            Series::Dirt => "hsl(93, 49%, 38%)",
        }
    }

    pub fn missing_image_color(&self) -> &'static str {
        "hsl(0, 0%, 90%)"
    }

    pub fn tile_label_color(&self) -> &'static str {
        "hsl(0, 0%, 30%)"
    }

    pub fn tile_count_color(&self) -> &'static str {
        self.map_border_color()
    }

    pub fn tile_number_color(&self) -> &'static str {
        self.map_title_color()
    }

    pub fn listing_background_color(&self) -> &'static str {
        "hsl(23, 53%, 94%)"
    }

    pub fn listing_border_color(&self) -> &'static str {
        self.map_border_color()
    }
}

impl Default for ExportColorScheme {
    fn default() -> Self {
        ExportColorScheme::new(Series::Dirt)
    }
}

impl fmt::Display for ExportColorScheme {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", self.series)
    }
}

impl FromStr for ExportColorScheme {
    type Err = crate::edition::ParseSeriesError;

    fn from_str(val: &str) -> std::result::Result<Self, Self::Err> {
        let series = Series::from_str(val)?;
        Ok(ExportColorScheme::new(series))
    }
}

//----------------------------------------------------------------------------

#[cfg(feature = "svg")]
pub mod svg {
    use ::svg::node::element::{Group, Image, Text};

    use crate::hexagon::{FloatCoordinate, FloatDirection, Layout};
    use crate::map::{PlacedTile, PlacedToken};
    use super::*;

    pub const BASE_URL_PATTERN: &str = "{base-url}";
    pub const TILE_ID_PATTERN: &str = "{tile-id}";
    pub const TOKEN_ID_PATTERN: &str = "{token-id}";

    pub struct TileImageGroup {
        inner: Group,
    }

    impl TileImageGroup {
        pub fn new(layout: &Layout, tile: &PlacedTile, image_url: &str) -> TileImageGroup {
            let pos = tile.pos.to_pixel(layout);
            let mut inner = Group::new()
                .set("class", "tile")
                .set("transform", format!("translate({:.3} {:.3})", pos.x(), pos.y()));

            let image_url = image_url
                .replace(TILE_ID_PATTERN, &format!("{:x}", tile.id()));
            let size = util::tile_image_size(layout);
            let center = util::tile_image_center(layout);
            let angle = layout.direction_to_angle(tile.dir);

            let image = Image::new()
                .set("href", image_url)
                .set("width", size.x())
                .set("height", size.y())
                .set("image-rendering", "optimizeQuality")
                .set("transform", format!("rotate({:.0}) translate({:.3} {:.3})", angle, -center.x(), -center.y()));
            inner = inner.add(image);

            TileImageGroup { inner }
        }
    }

    impl From<TileImageGroup> for Group {
        fn from(value: TileImageGroup) -> Group {
            value.inner
        }
    }

    pub struct TileLabelGroup {
        inner: Group,
    }

    impl TileLabelGroup {
        pub fn new(layout: &Layout, tile: &PlacedTile) -> TileLabelGroup {
            let pos = tile.pos.to_pixel(layout);
            let mut inner = Group::new()
                .set("class", "tile")
                .set("transform", format!("translate({:.3} {:.3})", pos.x(), pos.y()));

            let mut text = tile.id().base().to_string();
            if tile.has_flat_tokens() {
                text.push('*');
            }
            let label = Text::new()
                .set("class", "label")
                .set("x", 0)
                .set("y", 0)
                .add(::svg::node::Text::new(text));
            inner = inner.add(label);

            TileLabelGroup { inner }
        }
    }

    impl From<TileLabelGroup> for Group {
        fn from(value: TileLabelGroup) -> Group {
            value.inner
        }
    }

    pub struct TokenImageGroup {
        inner: Group,
    }

    impl TokenImageGroup {
        pub fn new(layout: &Layout, tile: &PlacedTile, token: &PlacedToken, image_url: &str) -> TokenImageGroup {
            let pos = (FloatCoordinate::from(tile.pos) + token.pos.rotate(tile.dir)).to_pixel(layout);
            let mut inner = Group::new()
                .set("class", "token")
                .set("transform", format!("translate({:.3} {:.3})", pos.x(), pos.y()));

            let image_url = image_url
                .replace(TILE_ID_PATTERN, &format!("{:x}", tile.id()))
                .replace(TOKEN_ID_PATTERN, &format!("{:x}", token.id));
            let size = util::token_image_size(layout, token.id);
            let center = util::token_image_center(layout, token.id);
            let angle = layout.direction_to_angle(FloatDirection::from(tile.dir) + token.dir);

            let image = Image::new()
                .set("href", image_url)
                .set("width", size.x())
                .set("height", size.y())
                .set("image-rendering", "optimizeQuality")
                .set("transform", format!("rotate({:.1}) translate({:.3} {:.3})", angle, -center.x(), -center.y()));
            inner = inner.add(image);

            TokenImageGroup { inner }
        }
    }

    impl From<TokenImageGroup> for Group {
        fn from(value: TokenImageGroup) -> Group {
            value.inner
        }
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

    #[test]
    fn export_scale_tile_size() {
        const BASE_SIZE: Point = Point(60.0, 60.0);

        let tile_size = ExportScale::Small.tile_size();
        assert_abs_diff_eq!(tile_size, BASE_SIZE * f32::from(ExportScale::Small));
        let tile_size = ExportScale::Medium.tile_size();
        assert_abs_diff_eq!(tile_size, BASE_SIZE * f32::from(ExportScale::Medium));
        let tile_size = ExportScale::Large.tile_size();
        assert_abs_diff_eq!(tile_size, BASE_SIZE * f32::from(ExportScale::Large));
        let tile_size = ExportScale::ExtraLarge.tile_size();
        assert_abs_diff_eq!(tile_size, BASE_SIZE * f32::from(ExportScale::ExtraLarge));
    }
}

//----------------------------------------------------------------------------
