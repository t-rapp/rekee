//----------------------------------------------------------------------------
//! Set of tiles that belong to the same game edition (core box or expansion).
//!
//! (Sidenote: I have not fully settled on the term "edition" yet; "box" might
//! be better but is already widely used for a different purpose in the Rust std
//! library. This type name might still change until the Rekee library reaches
//! version 1.0)
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use std::fmt;
use std::str::FromStr;

use serde::{Serialize, Deserialize};

use crate::tile::TileId;

//----------------------------------------------------------------------------

/// Rallyman game edition (core box or one of the expansions).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "kebab-case", try_from = "&str")]
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
    #[serde(rename = "dirt-110-percent")]
    Dirt110Percent,
    /// RX expansion for Rallyman: DIRT
    DirtRx,
    /// Climb expansion for Rallyman: DIRT
    DirtClimb,
    /// Copilot Pack expansion for Rallyman: DIRT
    DirtCopilotPack,
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
    #[deprecated(note = "Please use Series::editions() together with Edition::tiles() instead")]
    pub fn gt_tiles() -> Vec<TileId> {
        let mut tiles = Vec::with_capacity(145);
        for edition in Series::Gt.editions() {
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
    #[deprecated(note = "Please use Series::editions() together with Edition::tiles() instead")]
    pub fn dirt_tiles() -> Vec<TileId> {
        let mut tiles = Vec::with_capacity(162);
        for edition in Series::Dirt.editions() {
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
            Edition::DirtCopilotPack =>
                &DIRT_COPILOT_PACK,
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

    /// Check whether the edition contains a specific tile.
    ///
    /// Returns `true` when the identifier is found in the list of tiles.
    ///
    /// Compares the full tile identifier, including the graphical variant. To
    /// compare the base identifier only use [`Edition::contains_base()`]
    /// instead. See [`TileId::base()`] for more information on the difference
    /// between base and full tile identifiers.
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
    ///
    /// let edition = Edition::GtWorldTour;
    /// assert_eq!(edition.contains(tile!(124, a, 1)), false);
    /// assert_eq!(edition.contains(tile!(124, a, 2)), true);
    /// ```
    pub fn contains(&self, tile: TileId) -> bool {
        // clear side, any edition contains both sides of a tile
        let tile = TileId::new(tile.num(), 0, tile.var());
        // can use fast binary search as the list order is checked with unit tests
        self.internal_tiles().binary_search(&tile).is_ok()
    }

    /// Check whether the edition matches a given base tile identifier.
    ///
    /// Returns `true` when a tile with matching base identifier is found in the
    /// list of tiles.
    ///
    /// Compares the base tile identifier, with graphical variant excluded. To
    /// compare the full identifier use [`Edition::contains()`] instead. See
    /// [`TileId::base()`] for more information on the difference between base
    /// and full tile identifiers.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::edition::*;
    /// # use rekee::tile;
    /// let edition = Edition::DirtCoreBox;
    /// assert_eq!(edition.contains_base(tile!(201, a)), true);
    /// assert_eq!(edition.contains_base(tile!(325, b)), false);
    ///
    /// let edition = Edition::DirtClimb;
    /// assert_eq!(edition.contains_base(tile!(201, a)), false);
    /// assert_eq!(edition.contains_base(tile!(325, b)), true);
    ///
    /// let edition = Edition::GtWorldTour;
    /// assert_eq!(edition.contains_base(tile!(124, a, 1).base()), true);
    /// assert_eq!(edition.contains_base(tile!(124, a, 2).base()), true);
    /// ```
    pub fn contains_base(&self, tile: TileId) -> bool {
        // Ignore side as any edition contains both sides of a tile, and ignore
        // graphical variant as we care for the base identifier only
        let tile_num = tile.num();
        // can use fast binary search as the list order is checked with unit tests
        self.internal_tiles().binary_search_by_key(&tile_num, |tile| tile.num()).is_ok()
    }

    /// Check whether the edition is a stand-alone game, or an expansion module.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::edition::*;
    /// assert_eq!(Edition::GtAdrenalinePack.is_expansion(), true);
    /// assert_eq!(Edition::GtCoreBox.is_expansion(), false);
    /// ```
    pub fn is_expansion(&self) -> bool {
        !matches!(self, Edition::GtCoreBox | Edition::DirtCoreBox)
    }

    /// Game series that the edition belongs to.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::edition::*;
    /// let series = Edition::GtWorldTour.series();
    /// assert_eq!(series, Series::Gt);
    ///
    /// let series = Edition::Dirt110Percent.series();
    /// assert_eq!(series, Series::Dirt);
    /// ```
    pub fn series(&self) -> Series {
        if GT_EDITIONS.contains(self) {
            Series::Gt
        } else if DIRT_EDITIONS.contains(self) {
            Series::Dirt
        } else {
            unimplemented!();
        }
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
            Edition::DirtCopilotPack,
        ];
        EDITIONS.iter()
    }
}

impl fmt::Display for Edition {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Edition::GtCoreBox =>
                write!(fmt, "GT Core Box")?,
            Edition::GtChampionship =>
                write!(fmt, "GT Championship")?,
            Edition::GtWorldTour =>
                write!(fmt, "GT World Tour")?,
            Edition::GtTeamChallenge =>
                write!(fmt, "GT Team Challenge")?,
            Edition::GtAdrenalinePack =>
                write!(fmt, "GT Adrenaline Pack")?,
            Edition::DirtCoreBox =>
                write!(fmt, "DIRT Core Box")?,
            Edition::Dirt110Percent =>
                write!(fmt, "DIRT 110%")?,
            Edition::DirtRx =>
                write!(fmt, "DIRT RX")?,
            Edition::DirtClimb =>
                write!(fmt, "DIRT Climb")?,
            Edition::DirtCopilotPack =>
                write!(fmt, "DIRT Copilot Pack")?,
        }
        Ok(())
    }
}

// small hack to provide the Serde string serialization as a formatter
impl fmt::LowerHex for Edition {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut text = serde_json::to_string(self).unwrap();
        text.retain(|ch| ch != '"');
        write!(fmt, "{}", text)
    }
}

impl FromStr for Edition {
    type Err = ParseEditionError;

    fn from_str(val: &str) -> Result<Self, Self::Err> {
        // match strings from both trait implementations, std::fmt::Display and serde::Serialize
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
            "dirt-110-percent" | "dirt-110%" =>
                Ok(Edition::Dirt110Percent),
            "dirt-rx" =>
                Ok(Edition::DirtRx),
            "dirt-climb" =>
                Ok(Edition::DirtClimb),
            "dirt-copilot-pack" =>
                Ok(Edition::DirtCopilotPack),
            _ =>
                Err(ParseEditionError::Unknown(val.to_string())),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
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

impl std::convert::TryFrom<&str> for Edition {
    type Error = ParseEditionError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Edition::from_str(value)
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

const DIRT_COPILOT_PACK: [TileId; 6] = [
    tile!(416),
    tile!(417),
    tile!(902),
    tile!(903),
    tile!(904),
    tile!(905),
];

const GT_EDITIONS: [Edition; 5] = [
    Edition::GtCoreBox,
    Edition::GtChampionship,
    Edition::GtWorldTour,
    Edition::GtTeamChallenge,
    Edition::GtAdrenalinePack,
];

const DIRT_EDITIONS: [Edition; 5] = [
    Edition::DirtCoreBox,
    Edition::Dirt110Percent,
    Edition::DirtRx,
    Edition::DirtClimb,
    Edition::DirtCopilotPack,
];

//----------------------------------------------------------------------------

/// Rallyman game series (GT or DIRT).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Series {
    /// Rallyman: GT
    Gt,
    /// Rallyman: DIRT
    Dirt,
}

impl Series {
    /// Iterator over the editions of the game series.
    pub fn editions(&self) -> std::slice::Iter<'static, Edition> {
        let editions = match self {
            Series::Gt =>
                &GT_EDITIONS,
            Series::Dirt =>
                &DIRT_EDITIONS,
        };
        editions.iter()
    }

    /// Iterator over all game series.
    pub fn iter() -> std::slice::Iter<'static, Self> {
        const SERIES: [Series; 2] = [
            Series::Gt,
            Series::Dirt,
        ];
        SERIES.iter()
    }
}

impl fmt::Display for Series {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Series::Gt =>
                write!(fmt, "Rallyman: GT")?,
            Series::Dirt =>
                write!(fmt, "Rallyman: DIRT")?,
        }
        Ok(())
    }
}

impl FromStr for Series {
    type Err = ParseSeriesError;

    fn from_str(val: &str) -> Result<Self, Self::Err> {
        let mut s = String::from(val);
        s.make_ascii_lowercase();
        if s.ends_with("gt") {
            Ok(Series::Gt)
        } else if s.ends_with("dirt") {
            Ok(Series::Dirt)
        } else {
            Err(ParseSeriesError::Unknown(val.to_string()))
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseSeriesError {
    Unknown(String),
}

impl fmt::Display for ParseSeriesError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseSeriesError::Unknown(val) =>
                write!(fmt, "Unknown series token \"{}\"", val),
        }
    }
}

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
        tiles.extend_from_slice(&DIRT_COPILOT_PACK);
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
        tiles.extend_from_slice(&DIRT_COPILOT_PACK);
        for tile in &tiles {
            let count = tiles.iter()
                .filter(|id| *id == tile)
                .count();
            assert_eq!(count, 1, "tile {} is defined more than once", tile);
        }
    }

    #[test]
    fn edition_to_str() {
        assert_eq!(Edition::GtCoreBox.to_string(), "GT Core Box");
        assert_eq!(Edition::GtAdrenalinePack.to_string(), "GT Adrenaline Pack");
        assert_eq!(Edition::DirtCoreBox.to_string(), "DIRT Core Box");
        assert_eq!(Edition::Dirt110Percent.to_string(), "DIRT 110%");

        let text = format!("{:x}", Edition::GtChampionship);
        assert_eq!(text, "gt-championship");
        let text = format!("{:x}", Edition::DirtCopilotPack);
        assert_eq!(text, "dirt-copilot-pack");
    }

    #[test]
    fn edition_from_str() {
        assert_eq!("gt-world-tour".parse::<Edition>(), Ok(Edition::GtWorldTour));
        assert_eq!("GT Team Challenge".parse::<Edition>(), Ok(Edition::GtTeamChallenge));
        assert_eq!("dirt-110-percent".parse::<Edition>(), Ok(Edition::Dirt110Percent));
        assert_eq!("Dirt 110%".parse::<Edition>(), Ok(Edition::Dirt110Percent));
        assert_eq!("DIRT-RX".parse::<Edition>(), Ok(Edition::DirtRx));
        assert_eq!("dirt-copilot-pack".parse::<Edition>(), Ok(Edition::DirtCopilotPack));

        assert!("".parse::<Edition>().is_err());
        assert!("rx".parse::<Edition>().is_err());
    }

    #[test]
    fn edition_serde() {
        let edition = Edition::GtChampionship;
        let text = serde_json::to_string(&edition).unwrap();
        assert_eq!(text, r#""gt-championship""#);

        let edition = Edition::Dirt110Percent;
        let text = serde_json::to_string(&edition).unwrap();
        assert_eq!(text, r#""dirt-110-percent""#);

        let text = r#""Dirt Climb""#;
        let edition: Edition = serde_json::from_str(text).unwrap();
        assert_eq!(edition, Edition::DirtClimb);

        let text = r#""#;
        let result: Result<Edition, _> = serde_json::from_str(text);
        assert!(result.is_err());

        let text = r#""core-box""#;
        let result: Result<Edition, _> = serde_json::from_str(text);
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

        for edition in Edition::iter() {
            for tile in edition.tiles() {
                let info = match TileInfo::get(tile) {
                    Some(val) => val,
                    None => panic!("no tile info found for tile {}", tile),
                };
                if info.count() <= 1 {
                    assert!(tile.var() == 0, "tile {} is defined with non-empty variation", tile);
                } else {
                    assert!(tile.var() > 0, "tile {} is defined with empty variation", tile);
                }
                assert_eq!(edition.series(), info.series(), "series of tile {} does not match edition", tile);
            }
        }
    }

    #[test]
    fn series_to_str() {
        assert_eq!(Series::Gt.to_string(), "Rallyman: GT");
        assert_eq!(Series::Dirt.to_string(), "Rallyman: DIRT");
    }

    #[test]
    fn series_from_str() {
        assert_eq!("gt".parse::<Series>(), Ok(Series::Gt));
        assert_eq!("Rallyman GT".parse::<Series>(), Ok(Series::Gt));
        assert_eq!("DIRT".parse::<Series>(), Ok(Series::Dirt));
        assert_eq!("rallyman-dirt".parse::<Series>(), Ok(Series::Dirt));
        assert_eq!("Rallyman: DIRT".parse::<Series>(), Ok(Series::Dirt));

        assert!("".parse::<Series>().is_err());
        assert!("Rallyman".parse::<Series>().is_err());
    }
}

//----------------------------------------------------------------------------
