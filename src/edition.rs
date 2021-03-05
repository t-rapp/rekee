//----------------------------------------------------------------------------
//! Set of tiles that belong to the same game edition (core box or expansion).
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use crate::tile::TileId;

//----------------------------------------------------------------------------

/// Rallyman game edition (core box or one of the expansions).
pub enum Edition {
    /// Rallyman: GT core box
    GtCoreBox,
    /// Championship expansion for Rallyman: GT
    GtChampionship,
    /// World Tour expansion for Rallyman: GT
    GtWorldTour,
    /// Team Challenge expansion for Rallyman: GT
    GtTeamChallenge,
    /// Adrenaline Pack expansion for Rallyman: GT
    GtAdrenalinePack,
    /// Rallyman: DIRT core box
    DirtCoreBox,
}

impl Edition {
    /// Returns all the tiles of the game joined into a sorted list.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::edition::*;
    /// # use rekee::tile;
    /// let tiles = Edition::all_tiles();
    /// assert_eq!(tiles.first(), Some(&tile!(101)));
    /// assert_eq!(tiles.last(), Some(&tile!(901, b)));
    /// assert_eq!(tiles.len(), 145);
    /// ```
    pub fn all_tiles() -> Vec<TileId> {
        let mut tiles = Vec::with_capacity(145);
        for edition in Self::iter() {
            tiles.extend_from_slice(&edition.tiles());
        }
        tiles.sort_unstable();
        tiles
    }

    /// Returns the tiles of a specific game edition.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::edition::*;
    /// # use rekee::tile;
    /// let tiles = Edition::GtCoreBox.tiles();
    /// assert_eq!(tiles.first(), Some(&tile!(101)));
    /// assert_eq!(tiles.last(), Some(&tile!(120, b)));
    /// assert_eq!(tiles.len(), 63);
    /// ```
    pub fn tiles(&self) -> Vec<TileId> {
        let mut tiles = Vec::with_capacity(63);
        let iter = match self {
            Edition::GtCoreBox =>
                GT_CORE_BOX.iter(),
            Edition::GtChampionship =>
                GT_CHAMPIONSHIP.iter(),
            Edition::GtWorldTour =>
                GT_WORLD_TOUR.iter(),
            Edition::GtTeamChallenge =>
                GT_TEAM_CHALLENGE.iter(),
            Edition::GtAdrenalinePack =>
                GT_ADRENALINE_PACK.iter(),
            Edition::DirtCoreBox =>
                DIRT_CORE_BOX.iter(),
        };
        for tile in iter {
            let tile_a = TileId::new(tile.num(), 1, tile.var());
            let tile_b = TileId::new(tile.num(), 2, tile.var());
            // only add both sides if the tile actually has two different ones (not #101)
            if tile_a.base() != tile_b.base() {
                tiles.push(tile_a);
                tiles.push(tile_b);
            } else {
                tiles.push(*tile);
            }
        }
        tiles
    }

    /// Iterator over all game editions.
    pub fn iter() -> std::slice::Iter<'static, Self> {
        const EDITIONS: [Edition; 5] = [
            Edition::GtCoreBox,
            Edition::GtChampionship,
            Edition::GtWorldTour,
            Edition::GtTeamChallenge,
            Edition::GtAdrenalinePack,
            // TODO: add DIRT editions here once the user interface is ready
        ];
        EDITIONS.iter()
    }
}

//----------------------------------------------------------------------------

// Private variation of the public macro that skips tile a/b side information
macro_rules! tile {
    ($num:expr) => {
        $crate::tile::TileId::new($num, 0, 0)
    };
    ($num:expr, $var:expr) => {
        $crate::tile::TileId::new($num, 0, $var)
    };
}

const GT_CORE_BOX: [TileId; 32] = [
    tile!(101),
    tile!(102),
    tile!(103, 1), tile!(103, 2), tile!(103, 3),
    tile!(104, 1), tile!(104, 2), tile!(104, 3),
    tile!(105, 1), tile!(105, 2),
    tile!(106, 1), tile!(106, 2),
    tile!(107, 1), tile!(107, 2),
    tile!(108),
    tile!(109),
    tile!(110),
    tile!(111, 1), tile!(111, 2),
    tile!(112, 1), tile!(112, 2),
    tile!(113),
    tile!(114),
    tile!(115, 1), tile!(115, 2),
    tile!(116, 1), tile!(116, 2),
    tile!(117, 1), tile!(117, 2),
    tile!(118),
    tile!(119),
    tile!(120),
];

const GT_CHAMPIONSHIP: [TileId; 12] = [
    tile!(121, 1), tile!(121, 2),
    tile!(122, 1),
    tile!(123),
    tile!(124, 1),
    tile!(125, 1),
    tile!(126),
    tile!(127, 1), tile!(127, 2),
    tile!(128),
    tile!(129),
    tile!(130),
];

const GT_WORLD_TOUR: [TileId; 12] = [
    tile!(122, 2),
    tile!(124, 2),
    tile!(125, 2),
    tile!(131),
    tile!(132),
    tile!(133),
    tile!(134),
    tile!(135),
    tile!(136),
    tile!(137),
    tile!(138, 1), tile!(138, 2),
];

const GT_TEAM_CHALLENGE: [TileId; 12] = [
    tile!(139),
    tile!(140),
    tile!(141),
    tile!(142),
    tile!(143),
    tile!(144),
    tile!(145),
    tile!(146),
    tile!(147),
    tile!(148),
    tile!(149),
    tile!(150),
];

const GT_ADRENALINE_PACK: [TileId; 5] = [
    tile!(151),
    tile!(152),
    tile!(153),
    tile!(154),
    tile!(901),
];

const DIRT_CORE_BOX: [TileId; 32] = [
    tile!(201),
    tile!(202),
    tile!(203),
    tile!(204),
    tile!(205),
    tile!(206),
    tile!(207),
    tile!(208),
    tile!(209),
    tile!(210),
    tile!(211),
    tile!(212),
    tile!(213),
    tile!(214),
    tile!(215),
    tile!(216),
    tile!(217),
    tile!(218),
    tile!(219),
    tile!(220),
    tile!(221),
    tile!(222),
    tile!(223),
    tile!(224),
    tile!(225),
    tile!(226),
    tile!(227),
    tile!(228),
    tile!(229),
    tile!(230),
    tile!(231),
    tile!(232),
];

//----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tile_list_empty_side() {
        let mut tiles = Vec::new();
        tiles.extend_from_slice(&GT_CORE_BOX);
        tiles.extend_from_slice(&GT_CHAMPIONSHIP);
        tiles.extend_from_slice(&GT_WORLD_TOUR);
        tiles.extend_from_slice(&GT_TEAM_CHALLENGE);
        tiles.extend_from_slice(&GT_ADRENALINE_PACK);
        tiles.extend_from_slice(&DIRT_CORE_BOX);
        for tile in &tiles {
            assert_eq!(tile.side(), 0, "tile {} is defined with a non-empty side", tile);
        }
    }

    #[test]
    fn tile_list_unique() {
        let mut tiles = Vec::new();
        tiles.extend_from_slice(&GT_CORE_BOX);
        tiles.extend_from_slice(&GT_CHAMPIONSHIP);
        tiles.extend_from_slice(&GT_WORLD_TOUR);
        tiles.extend_from_slice(&GT_TEAM_CHALLENGE);
        tiles.extend_from_slice(&GT_ADRENALINE_PACK);
        tiles.extend_from_slice(&DIRT_CORE_BOX);
        for tile in &tiles {
            let count = tiles.iter()
                .filter(|id| *id == tile)
                .count();
            assert_eq!(count, 1, "tile {} is defined more than once", tile);
        }
    }

    #[test]
    fn edition_tiles_match_info() {
        use crate::tile::TileInfo;

        let tiles = Edition::all_tiles();
        for tile in &tiles {
            let info = match TileInfo::get(*tile) {
                Some(val) => val,
                None => panic!("no tile info found for tile {}", tile),
            };
            if info.count() <= 1 {
                assert!(tile.var() == 0, "tile {} is defined with non-empty variation", tile);
            } else {
                assert!(tile.var() > 0, "tile {} is defined with empty variation", tile);
            }
        }
    }
}

//----------------------------------------------------------------------------
