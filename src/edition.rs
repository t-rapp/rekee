//----------------------------------------------------------------------------
//! Set of tiles that belong to the same game edition (core box or expansion).
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use std::fmt;
use std::str::FromStr;

use serde::{Serialize, Serializer, Deserialize, Deserializer};
use serde::de::{self, Visitor};

use crate::tile::TileId;

//----------------------------------------------------------------------------

/// Rallyman game edition (core box or one of the expansions).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
    /// 110% expansion for Rallyman: DIRT
    Dirt110Percent,
    /// RX expansion for Rallyman: DIRT
    DirtRx,
    /// Climb expansion for Rallyman: DIRT
    DirtClimb,
    /// Copilot expansion for Rallyman: DIRT
    DirtCopilot,
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
    /// assert_eq!(tiles.last(), Some(&tile!(905, b)));
    /// assert_eq!(tiles.len(), 307);
    /// ```
    pub fn all_tiles() -> Vec<TileId> {
        let mut tiles = Vec::with_capacity(307);
        for edition in Self::iter() {
            tiles.extend_from_slice(&edition.tiles());
        }
        tiles.sort_unstable();
        tiles
    }

    /// Returns tiles part of Rallyman: GT game editions joined into a sorted list.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::edition::*;
    /// # use rekee::tile;
    /// let tiles = Edition::gt_tiles();
    /// assert_eq!(tiles.first(), Some(&tile!(101)));
    /// assert_eq!(tiles.last(), Some(&tile!(901, b)));
    /// assert_eq!(tiles.len(), 145);
    /// ```
    pub fn gt_tiles() -> Vec<TileId> {
        const EDITIONS: [Edition; 5] = [
            Edition::GtCoreBox,
            Edition::GtChampionship,
            Edition::GtWorldTour,
            Edition::GtTeamChallenge,
            Edition::GtAdrenalinePack,
        ];
        let mut tiles = Vec::with_capacity(145);
        for edition in EDITIONS.iter() {
            tiles.extend_from_slice(&edition.tiles());
        }
        tiles.sort_unstable();
        tiles
    }

    /// Returns tiles part of Rallyman: DIRT game editions joined into a sorted list.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::edition::*;
    /// # use rekee::tile;
    /// let tiles = Edition::dirt_tiles();
    /// assert_eq!(tiles.first(), Some(&tile!(201, a)));
    /// assert_eq!(tiles.last(), Some(&tile!(905, b)));
    /// assert_eq!(tiles.len(), 162);
    /// ```
    pub fn dirt_tiles() -> Vec<TileId> {
        const EDITIONS: [Edition; 5] = [
            Edition::DirtCoreBox,
            Edition::Dirt110Percent,
            Edition::DirtRx,
            Edition::DirtClimb,
            Edition::DirtCopilot,
        ];
        let mut tiles = Vec::with_capacity(162);
        for edition in EDITIONS.iter() {
            tiles.extend_from_slice(&edition.tiles());
        }
        tiles.sort_unstable();
        tiles
    }

    fn internal_tiles(&self) -> &'static [TileId] {
        match self {
            Edition::GtCoreBox =>
                &GT_CORE_BOX,
            Edition::GtChampionship =>
                &GT_CHAMPIONSHIP,
            Edition::GtWorldTour =>
                &GT_WORLD_TOUR,
            Edition::GtTeamChallenge =>
                &GT_TEAM_CHALLENGE,
            Edition::GtAdrenalinePack =>
                &GT_ADRENALINE_PACK,
            Edition::DirtCoreBox =>
                &DIRT_CORE_BOX,
            Edition::Dirt110Percent =>
                &DIRT_110_PERCENT,
            Edition::DirtRx =>
                &DIRT_RX,
            Edition::DirtClimb =>
                &DIRT_CLIMB,
            Edition::DirtCopilot =>
                &DIRT_COPILOT,
        }
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
        for tile in self.internal_tiles().iter() {
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

    /// Check whether a specific game edition contains the given tile.
    ///
    /// Returns `true` when the identifier is found in the list of tiles.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::edition::*;
    /// # use rekee::tile;
    /// let edition = Edition::DirtCoreBox;
    /// assert_eq!(edition.contains(tile!(201, a)), true);
    /// assert_eq!(edition.contains(tile!(325, b)), false);
    ///
    /// let edition = Edition::DirtClimb;
    /// assert_eq!(edition.contains(tile!(201, a)), false);
    /// assert_eq!(edition.contains(tile!(325, b)), true);
    /// ```
    pub fn contains(&self, tile: TileId) -> bool {
        // clear side, any edition contains both sides of a tile
        let tile = TileId::new(tile.num(), 0, tile.var());
        // can use fast binary search as the list order is checked with unit tests
        self.internal_tiles().binary_search(&tile).is_ok()
    }

    /// Iterator over all game editions.
    pub fn iter() -> std::slice::Iter<'static, Self> {
        const EDITIONS: [Edition; 10] = [
            Edition::GtCoreBox,
            Edition::GtChampionship,
            Edition::GtWorldTour,
            Edition::GtTeamChallenge,
            Edition::GtAdrenalinePack,
            Edition::DirtCoreBox,
            Edition::Dirt110Percent,
            Edition::DirtRx,
            Edition::DirtClimb,
            Edition::DirtCopilot,
        ];
        EDITIONS.iter()
    }
}

impl fmt::Display for Edition {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Edition::GtCoreBox =>
                write!(fmt, "gt-core-box")?,
            Edition::GtChampionship =>
                write!(fmt, "gt-championship")?,
            Edition::GtWorldTour =>
                write!(fmt, "gt-world-tour")?,
            Edition::GtTeamChallenge =>
                write!(fmt, "gt-team-challenge")?,
            Edition::GtAdrenalinePack =>
                write!(fmt, "gt-adrenaline-pack")?,
            Edition::DirtCoreBox =>
                write!(fmt, "dirt-core-box")?,
            Edition::Dirt110Percent =>
                write!(fmt, "dirt-110-percent")?,
            Edition::DirtRx =>
                write!(fmt, "dirt-rx")?,
            Edition::DirtClimb =>
                write!(fmt, "dirt-climb")?,
            Edition::DirtCopilot =>
                write!(fmt, "dirt-copilot")?,
        }
        Ok(())
    }
}

impl FromStr for Edition {
    type Err = ParseEditionError;

    fn from_str(val: &str) -> Result<Self, Self::Err> {
        let mut s = val.replace(char::is_whitespace, "-");
        s.make_ascii_lowercase();
        match s.as_ref() {
            "gt-core-box" =>
                Ok(Edition::GtCoreBox),
            "gt-championship" =>
                Ok(Edition::GtChampionship),
            "gt-world-tour" =>
                Ok(Edition::GtWorldTour),
            "gt-team-challenge" =>
                Ok(Edition::GtTeamChallenge),
            "gt-adrenaline-pack" =>
                Ok(Edition::GtAdrenalinePack),
            "dirt-core-box" =>
                Ok(Edition::DirtCoreBox),
            "dirt-110-percent" =>
                Ok(Edition::Dirt110Percent),
            "dirt-rx" =>
                Ok(Edition::DirtRx),
            "dirt-climb" =>
                Ok(Edition::DirtClimb),
            "dirt-copilot" =>
                Ok(Edition::DirtCopilot),
            _ =>
                Err(ParseEditionError::Unknown(val.to_string())),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseEditionError {
    Unknown(String),
}

impl fmt::Display for ParseEditionError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseEditionError::Unknown(val) =>
                write!(fmt, "Unknown edition token \"{}\"", val),
        }
    }
}

impl Serialize for Edition {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for Edition {
    fn deserialize<D>(deserializer: D) -> Result<Edition, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct EditionVisitor;

        impl<'de> Visitor<'de> for EditionVisitor {
            type Value = Edition;

            fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
                fmt.write_str("an edition identifier string")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match Edition::from_str(value) {
                    Ok(val) => Ok(val),
                    Err(_) => Err(E::custom(format!("invalid edition identifier: {}", value))),
                }
            }
        }

        deserializer.deserialize_string(EditionVisitor)
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

const DIRT_110_PERCENT: [TileId; 24] = [
    tile!(301),
    tile!(302),
    tile!(303),
    tile!(304),
    tile!(305),
    tile!(306),
    tile!(307),
    tile!(308),
    tile!(309),
    tile!(310),
    tile!(311),
    tile!(312),
    tile!(313),
    tile!(314),
    tile!(315),
    tile!(316),
    tile!(317),
    tile!(318),
    tile!(319),
    tile!(320),
    tile!(321),
    tile!(322),
    tile!(323),
    tile!(324),
];

const DIRT_RX: [TileId; 15] = [
    tile!(401),
    tile!(402),
    tile!(403),
    tile!(404),
    tile!(405),
    tile!(406),
    tile!(407),
    tile!(408),
    tile!(409),
    tile!(410),
    tile!(411),
    tile!(412),
    tile!(413),
    tile!(414),
    tile!(415),
];

const DIRT_CLIMB: [TileId; 4] = [
    tile!(325),
    tile!(326),
    tile!(418),
    tile!(419),
];

const DIRT_COPILOT: [TileId; 6] = [
    tile!(416),
    tile!(417),
    tile!(902),
    tile!(903),
    tile!(904),
    tile!(905),
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
        tiles.extend_from_slice(&DIRT_110_PERCENT);
        tiles.extend_from_slice(&DIRT_RX);
        tiles.extend_from_slice(&DIRT_CLIMB);
        tiles.extend_from_slice(&DIRT_COPILOT);
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
        tiles.extend_from_slice(&DIRT_110_PERCENT);
        tiles.extend_from_slice(&DIRT_RX);
        tiles.extend_from_slice(&DIRT_CLIMB);
        tiles.extend_from_slice(&DIRT_COPILOT);
        for tile in &tiles {
            let count = tiles.iter()
                .filter(|id| *id == tile)
                .count();
            assert_eq!(count, 1, "tile {} is defined more than once", tile);
        }
    }

    #[test]
    fn edition_to_str() {
        assert_eq!(Edition::GtCoreBox.to_string(), "gt-core-box");
        assert_eq!(Edition::GtAdrenalinePack.to_string(), "gt-adrenaline-pack");
        assert_eq!(Edition::DirtCoreBox.to_string(), "dirt-core-box");
        assert_eq!(Edition::Dirt110Percent.to_string(), "dirt-110-percent");
    }

    #[test]
    fn edition_from_str() {
        assert_eq!("gt-world-tour".parse::<Edition>(), Ok(Edition::GtWorldTour));
        assert_eq!("GT Team Challenge".parse::<Edition>(), Ok(Edition::GtTeamChallenge));
        assert_eq!("DIRT-RX".parse::<Edition>(), Ok(Edition::DirtRx));
        assert_eq!("dirt-copilot".parse::<Edition>(), Ok(Edition::DirtCopilot));

        assert!("".parse::<Edition>().is_err());
        assert!("rx".parse::<Edition>().is_err());
    }

    #[test]
    fn edition_serde() {
        let edition = Edition::GtChampionship;
        let text = serde_json::to_string(&edition).unwrap();
        assert_eq!(text, r#""gt-championship""#);

        let text = r#""Dirt Climb""#;
        let edition: Edition = serde_json::from_str(&text).unwrap();
        assert_eq!(edition, Edition::DirtClimb);

        let text = r#""#;
        let result: Result<Edition, _> = serde_json::from_str(&text);
        assert!(result.is_err());

        let text = r#""core-box""#;
        let result: Result<TileId, _> = serde_json::from_str(&text);
        assert!(result.is_err());
    }

    #[test]
    fn edition_tiles_sorted() {
        for edition in Edition::iter() {
            for tiles in edition.internal_tiles().windows(2) {
                assert!(tiles[0] <= tiles[1], "tile list is not strictly sorted ({} > {})", tiles[0], tiles[1]);
            }
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
