//----------------------------------------------------------------------------
//! Utilitiy functions for map image export.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
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
