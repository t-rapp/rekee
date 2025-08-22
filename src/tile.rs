//----------------------------------------------------------------------------
//! Tile identifier and characteristics.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use std::cmp::Reverse;
use std::collections::{BTreeMap, HashSet};
use std::collections::btree_map::Iter;
use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

use serde::{Serialize, Serializer, Deserialize};

use crate::edition::{Edition, Series};
use crate::hexagon::Direction;

//----------------------------------------------------------------------------

/// Identifier of a game tile.
///
/// Each tile identifier consists of three parts:
/// * a three digit catalog number `num` (101 .. 999)
/// * a `side` number for the tile front (a => 1) or back (b => 2)
/// * a tile variant `var`, if there are multiple graphical variants of a tile
///
/// # Examples
///
/// ```
/// # use rekee::tile::*;
/// let tile: TileId = "102a".parse().unwrap();
/// assert_eq!(tile.num(), 102);
/// assert_eq!(tile.side(), 1);
/// assert_eq!(tile.var(), 0);
///
/// let tile = TileId::new(103, 1, 2);
/// assert_eq!(tile.to_string(), "103a-2");
/// ```
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[derive(Deserialize)]
#[serde(try_from = "&str")]
pub struct TileId {
    num: u16,
    side: u8,
    var: u8,
}

impl TileId {
    /// Creates a new tile identifier from catalog number, tile side, and variant.
    pub const fn new(num: u16, side: u8, var: u8) -> Self {
        TileId { num, side, var }
    }

    /// Tile catalog number.
    pub fn num(&self) -> u16 {
        self.num
    }

    /// Tile side (1 => front / a, 2 => back / b).
    ///
    /// Side can be zero if tile front and back are the same.
    pub fn side(&self) -> u8 {
        self.side
    }

    /// Graphical tile variant.
    ///
    /// Variant number can be zero if there is only a single variant existing.
    pub fn var(&self) -> u8 {
        self.var
    }

    /// Returns the "base" identifier of a game tile. This is the identifier
    /// that describes all the tile characteristics with graphical variant `var`
    /// stripped off, and also `side` if the tile has the same printing on front
    /// and back.
    ///
    /// The base identifier is used for track setup tile lists, for example.
    pub fn base(&self) -> Self {
        let side = match self.num {
            // pitstop tile is the same on front and back
            101 => 0,
            // all other tiles have two different sides
            _ => self.side,
        };
        TileId { num: self.num, side, var: 0 }
    }
}

impl fmt::Display for TileId {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}", self.num)?;
        match self.side {
            0 => (),
            1 => write!(fmt, "a")?,
            2 => write!(fmt, "b")?,
            _ => unimplemented!(),
        }
        if self.var > 0 {
            write!(fmt, "-{}", self.var)?;
        }
        Ok(())
    }
}

// small hack to provide the Serde string serialization as a formatter
impl fmt::LowerHex for TileId {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut text = serde_json::to_string(self).unwrap();
        text.retain(|ch| ch != '"');
        write!(fmt, "{}", text)
    }
}

impl FromStr for TileId {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // check for tile alias
        if s.eq_ignore_ascii_case("filler1a") {
            return Ok(TileId::new(902, 1, 0));
        } else if s.eq_ignore_ascii_case("filler1b") {
            return Ok(TileId::new(902, 2, 0));
        } else if s.eq_ignore_ascii_case("filler2a") {
            return Ok(TileId::new(903, 1, 0));
        } else if s.eq_ignore_ascii_case("filler2b") {
            return Ok(TileId::new(903, 2, 0));
        } else if s.eq_ignore_ascii_case("filler3a") {
            return Ok(TileId::new(904, 1, 0));
        } else if s.eq_ignore_ascii_case("filler3b") {
            return Ok(TileId::new(904, 2, 0));
        } else if s.eq_ignore_ascii_case("podium") {
            return Ok(TileId::new(905, 1, 0));
        }
        // parse string pattern "{num}{side}-{var}"
        let mut idx = s.len().min(3);
        let num = s[0..idx].parse::<u16>()?;
        let side = match s.get(idx..idx+1) {
            None => 0,
            Some("a") | Some("A") => { idx += 1; 1},
            Some("b") | Some("B") => { idx += 1; 2},
            Some(_) => 0,
        };
        if Some("-") == s.get(idx..idx+1) {
            idx += 1;
        }
        let var = if s.len() > idx {
            s[idx..].parse::<u8>()?
        } else {
            0
        };
        Ok(TileId { num, side, var })
    }
}

impl std::convert::TryFrom<&str> for TileId {
    type Error = ParseIntError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        TileId::from_str(value)
    }
}

impl Serialize for TileId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl AsRef<TileId> for TileId {
    fn as_ref(&self) -> &TileId {
        self
    }
}

/// Creates a new tile identifier.
///
/// The `tile!` macro adds convenience on directly calling `TileId::new()` as it
/// allows to skip `side` or `var` parameters, if unused. Also the value for
/// parameter `side` is restricted to `a` and `b` to avoid invalid tile
/// identifiers.
///
/// # Examples
///
/// ```
/// # #[macro_use] extern crate rekee;
/// # use rekee::tile::TileId;
/// # fn main() {
/// let tile: TileId = tile!(102, a);
/// assert_eq!(tile.num(), 102);
/// assert_eq!(tile.side(), 1);
/// assert_eq!(tile.var(), 0);
/// # }
/// ```
#[macro_export]
macro_rules! tile {
    ($num:expr) => {
        $crate::tile::TileId::new($num, 0, 0)
    };
    ($num:expr, a) => {
        $crate::tile::TileId::new($num, 1, 0)
    };
    ($num:expr, b) => {
        $crate::tile::TileId::new($num, 2, 0)
    };
    ($num:expr, a, $var:expr) => {
        $crate::tile::TileId::new($num, 1, $var)
    };
    ($num:expr, b, $var:expr) => {
        $crate::tile::TileId::new($num, 2, $var)
    };
}

//----------------------------------------------------------------------------

/// Helper that counts tiles per catalog number.
struct TileCount {
    inner: BTreeMap::<u16, u32>,
}

impl TileCount {
    fn from_tiles<T: AsRef<TileId>>(tiles: &[T]) -> Self {
        let mut list = BTreeMap::new();
        for tile in tiles {
            let tile_num = tile.as_ref().num();
            let count = list.entry(tile_num).or_insert(0);
            *count += 1;
        }
        TileCount { inner: list }
    }

    fn from_edition(edition: Edition) -> Self {
        let mut list = BTreeMap::new();
        for tile in edition.tiles() {
            if tile.side() > 1 {
                continue; // only count each tile once per a/b side
            }
            let tile_num = tile.num();
            let count = list.entry(tile_num).or_insert(0);
            *count += 1;
        }
        TileCount { inner: list }
    }

    fn iter(&self) -> Iter<'_, u16, u32> {
        self.inner.iter()
    }

    fn intersection(&self, other: &Self) -> Self {
        let mut list = BTreeMap::new();
        for (tile_num, count) in self.inner.iter() {
            if let Some(other_count) = other.inner.get(tile_num) {
                let count = count.min(other_count);
                if *count > 0 {
                    list.insert(*tile_num, *count);
                }
            }
        }
        TileCount { inner: list }
    }

    fn total_count(&self) -> u32 {
        let mut total_count = 0;
        for (_, count) in self.inner.iter() {
            total_count += count;
        }
        total_count
    }
}

//----------------------------------------------------------------------------

/// Helper that counts common tiles per catalog number between a single edition
/// and another list of tiles.
struct EditionTileCount {
    edition: Edition,
    common: TileCount,
}

impl EditionTileCount {
    fn new(edition: Edition, list_tiles: &TileCount) -> Self {
        let common = TileCount::from_edition(edition)
            .intersection(list_tiles);
        EditionTileCount { edition, common }
    }
}

//----------------------------------------------------------------------------

/// An iterator that yields the editions that are necessary to assemble a list
/// of tiles.
///
/// This struct is created by the [`group_by_edition`] method on [`TileList`].
/// See its documentation for more.
///
/// [`group_by_edition`]: TileList::group_by_edition
pub struct GroupByEdition {
    editions: Vec<EditionTileCount>,
    edition_offset: usize,
    tiles_remaining: Vec<TileId>,
    tiles_used: Vec<TileId>,
}

impl GroupByEdition {
    fn new<T: AsRef<TileId> + Clone>(list: &[T]) -> Self {
        let list_tiles = TileCount::from_tiles(list);
        let mut editions: Vec<_> = Edition::iter()
            .map(|&edition| EditionTileCount::new(edition, &list_tiles))
            .filter(|item| !item.common.inner.is_empty())
            .collect();
        editions.sort_by_cached_key(|item| Reverse(item.common.total_count()));
        let tiles_remaining: Vec<_> = list.iter()
            .map(|val| *val.as_ref())
            .collect();
        let tiles_used = Vec::with_capacity(tiles_remaining.len());
        GroupByEdition { editions, edition_offset: 0, tiles_remaining, tiles_used }
    }

    /// Returns the subset of tiles that are part of the current edition.
    ///
    /// The result of this method is updated with each call to [`next`]. Each
    /// time the iterator yields `Some(Edition)` the according tiles are
    /// returned. Once the iterator reaches `None` it returns the list of
    /// remaining tiles that are not part of any edition.
    ///
    /// Note that the order of tiles returned by this method depends on internal
    /// implementation. There is no guarantee that the order of the returned
    /// subset matches the original order of the tile list.
    ///
    /// See [`group_by_edition`] for some code examples.
    ///
    /// [`next`]: Iterator::next
    /// [`group_by_edition`]: TileList::group_by_edition
    pub fn tiles(&self) -> &[TileId] {
        self.tiles_used.as_ref()
    }
}

impl Iterator for GroupByEdition {
    type Item = Edition;

    fn next(&mut self) -> Option<Edition> {
        let mut edition = None;
        self.tiles_used.clear();
        // iterate through all editions once with offset
        for item in self.editions.iter()
            .cycle()
            .skip(self.edition_offset)
            .take(self.editions.len())
        {
            for (&tile_num, &count) in item.common.iter() {
                for _ in 0..count {
                    let index = self.tiles_remaining.iter()
                        .position(|tile| tile.num() == tile_num);
                    if let Some(index) = index {
                        let tile = self.tiles_remaining.swap_remove(index);
                        self.tiles_used.push(tile);
                    }
                }
            }
            // return first edition that has tiles used
            if !self.tiles_used.is_empty() {
                edition = Some(item.edition);
                break;
            }
        }
        if edition.is_none() {
            // report tiles that do not match any edition when reaching end of iteration
            self.tiles_used.append(&mut self.tiles_remaining);
        }
        // increment offset after each call
        self.edition_offset += 1;
        edition
     }
}

//----------------------------------------------------------------------------

/// An iterator that yields the terrain surfaces that are included in a list of
/// tiles.
///
/// This struct is created by the [`group_by_terrain`] method on [`TileList`].
/// See its documentation for more.
///
/// [`group_by_terrain`]: TileList::group_by_terrain
pub struct GroupByTerrain {
    terrains: Vec<Option<Terrain>>,
    tiles: Vec<TileId>,
    index: usize,
    next_index: usize,
}

impl GroupByTerrain {
    fn new<T: AsRef<TileId> + Clone>(list: &[T]) -> Self {
        // collect terrain surface information for all tiles
        let mut terrain_tiles: Vec<_> = list.iter()
            .map(|item| {
                let tile_id = *item.as_ref();
                let terrain = TileInfo::get(tile_id)
                    .map(|info| info.terrain());
                (terrain, tile_id)
            })
            .collect();
        // when sorting make sure the tile count for unknown tiles appears at the end
        terrain_tiles.sort_by_key(|(terrain, _)| (terrain.is_none(), *terrain));

        // split the combined list into two separate lists once, for less
        // overhead in the tiles() function implementation
        let mut terrains = Vec::with_capacity(terrain_tiles.len());
        let mut tiles = Vec::with_capacity(terrain_tiles.len());
        for (terrain, tile_id) in terrain_tiles {
            terrains.push(terrain);
            tiles.push(tile_id);
        }

        GroupByTerrain { terrains, tiles, index: 0, next_index: 0 }
    }

    fn find_next_index(&self) -> Option<usize> {
        let terrain = self.terrains.get(self.index)?;
        let mut index = self.index;
        loop {
            index += 1;
            let next_surface = match self.terrains.get(index) {
                Some(val) => val,
                None => break,
            };
            if next_surface != terrain {
                break;
            }
        }
        Some(index)
    }

    /// Returns the subset of tiles that belong to the current terrain surface.
    ///
    /// The result of this method is updated upon each call to [`next`]. Each
    /// time the iterator yields `Some(Terrain)` the according tiles are
    /// returned. Tiles where no internal terrain surface information exists are
    /// returned once the iterator yields `None`.
    ///
    /// Note that the order of tiles returned by this method depends on internal
    /// implementation. There is no guarantee that the order of the returned
    /// subset matches the original order of the tile list.
    ///
    /// See [`group_by_terrain`] for some code examples.
    ///
    /// [`next`]: Iterator::next
    /// [`group_by_terrain`]: TileList::group_by_terrain
    pub fn tiles(&self) -> &[TileId] {
        debug_assert_eq!(self.terrains.len(), self.tiles.len());
        assert!(self.index <= self.next_index);
        if self.index < self.tiles.len() {
            &self.tiles[self.index..self.next_index]
        } else {
            &[][..]
        }
    }
}

impl Iterator for GroupByTerrain {
    type Item = Terrain;

    fn next(&mut self) -> Option<Terrain> {
        self.index = self.next_index;
        if let Some(terrain) = self.terrains.get(self.index) {
            self.next_index = self.find_next_index().unwrap_or(self.index);
            *terrain
        } else {
            None
        }
    }
}

//----------------------------------------------------------------------------

/// An iterator that yields the danger levels that are included in a list of
/// tiles.
///
/// This struct is created by the [`group_by_danger_level`] method on
/// [`TileList`].  See its documentation for more.
///
/// [`group_by_danger_level`]: TileList::group_by_danger_level
pub struct GroupByDangerLevel {
    danger_levels: Vec<Option<DangerLevel>>,
    tiles: Vec<TileId>,
    index: usize,
    next_index: usize,
}

impl GroupByDangerLevel {
    fn new<T: AsRef<TileId> + Clone>(list: &[T]) -> Self {
        // collect terrain danger level information for all tiles
        let mut danger_level_tiles: Vec<_> = list.iter()
            .map(|item| {
                let tile_id = *item.as_ref();
                let danger_level = TileInfo::get(tile_id)
                    .map(|info| info.danger_level());
                (danger_level, tile_id)
            })
            .collect();
        // when sorting make sure the entries for unknown tiles appear at the end
        danger_level_tiles.sort_by_key(|(danger_level, _)| (danger_level.is_none(), *danger_level));

        // split the combined list into two separate lists once, for less
        // overhead in the tiles() function implementation
        let mut danger_levels = Vec::with_capacity(danger_level_tiles.len());
        let mut tiles = Vec::with_capacity(danger_level_tiles.len());
        for (danger_level, tile_id) in danger_level_tiles {
            danger_levels.push(danger_level);
            tiles.push(tile_id);
        }

        GroupByDangerLevel { danger_levels, tiles, index: 0, next_index: 0 }
    }

    fn find_next_index(&self) -> Option<usize> {
        let danger_level = self.danger_levels.get(self.index)?;
        let mut index = self.index;
        loop {
            index += 1;
            let next_danger_level = match self.danger_levels.get(index) {
                Some(val) => val,
                None => break,
            };
            if next_danger_level != danger_level {
                break;
            }
        }
        Some(index)
    }

    /// Returns the subset of tiles that share the same danger level.
    ///
    /// The result of this method is updated upon each call to [`next`]. Each
    /// time the iterator yields `Some(DangerLevel)` the according tiles are
    /// returned. Tiles where no internal terrain information exists are
    /// returned once the iterator yields `None`.
    ///
    /// Note that the order of tiles returned by this method depends on internal
    /// implementation. There is no guarantee that the order of the returned
    /// subset matches the original order of the tile list.
    ///
    /// See [`group_by_danger_level`] for some code examples.
    ///
    /// [`next`]: Iterator::next
    /// [`group_by_danger_level`]: TileList::group_by_danger_level
    pub fn tiles(&self) -> &[TileId] {
        debug_assert_eq!(self.danger_levels.len(), self.tiles.len());
        assert!(self.index <= self.next_index);
        if self.index < self.tiles.len() {
            &self.tiles[self.index..self.next_index]
        } else {
            &[][..]
        }
    }
}

impl Iterator for GroupByDangerLevel {
    type Item = DangerLevel;

    fn next(&mut self) -> Option<DangerLevel> {
        self.index = self.next_index;
        if let Some(danger_level) = self.danger_levels.get(self.index) {
            self.next_index = self.find_next_index().unwrap_or(self.index);
            *danger_level
        } else {
            None
        }
    }
}

//----------------------------------------------------------------------------

/// Count information for a specific base tile.
///
/// This struct is created by the [`tile_summary`] method on [`TileList`].
/// See its documentation for more.
///
/// [`tile_summary`]: TileList::tile_summary
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TileSummary {
    pub tile: TileId,
    pub count: u32,
}

impl TileSummary {
    const fn new(tile: TileId, count: u32) -> Self {
        TileSummary { tile, count }
    }
}

//----------------------------------------------------------------------------

/// Edition and tile count information for a specific edition.
///
/// This struct is created by the [`edition_summary`] method on [`TileList`].
/// See its documentation for more.
///
/// [`edition_summary`]: TileList::edition_summary
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EditionSummary {
    pub edition: Option<Edition>,
    pub edition_count: u32,
    pub tile_count: u32,
}

impl EditionSummary {
    const fn new(edition: Option<Edition>, edition_count: u32, tile_count: u32) -> Self {
        EditionSummary { edition, edition_count, tile_count }
    }
}

//----------------------------------------------------------------------------

/// Tile count information for a specific terrain surface.
///
/// This struct is created by the [`terrain_summary`] method on [`TileList`].
///
/// [`terrain_summary`]: TileList::terrain_summary
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TerrainSummary {
    pub terrain: Option<Terrain>,
    pub tile_count: u32,
}

impl TerrainSummary {
    const fn new(terrain: Option<Terrain>, tile_count: u32) -> Self {
        TerrainSummary { terrain, tile_count }
    }
}

//----------------------------------------------------------------------------

/// Tile count information for a specific terrain danger level.
///
/// This struct is created by the [`danger_level_summary`] method on [`TileList`].
///
/// [`danger_level_summary`]: TileList::danger_level_summary
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DangerLevelSummary {
    pub danger_level: Option<DangerLevel>,
    pub tile_count: u32,
}

impl DangerLevelSummary {
    const fn new(danger_level: Option<DangerLevel>, tile_count: u32) -> Self {
        DangerLevelSummary { danger_level, tile_count }
    }
}

//----------------------------------------------------------------------------

/// Utilitiy methods on any list of [`TileId`]s.
pub trait TileList {

    /// Returns an iterator over the editions that are necessary to build the
    /// current list of tiles.
    ///
    /// Editions can occur multiple times during the iteration if more instances
    /// of a tile are used than what is provided by a single edition. Only the
    /// tile catalog number [`num`](TileId::num) is relevant for matching tiles
    /// and editions, [`side`](TileId::side) and [`var`](TileId::var) are
    /// ignored.
    ///
    /// The order of editions returned by the iterator depends on the internal
    /// implementation.
    ///
    /// Upon each step of the iterator the group of tiles that belong to the
    /// current edition is available in [`tiles`](GroupByEdition::tiles).
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::edition::Edition;
    /// # use rekee::tile::{TileId, TileList};
    /// let tiles = vec![tile!(101), tile!(103, a, 1), tile!(124, a), tile!(124, b)];
    /// let mut iter = tiles.group_by_edition();
    ///
    /// assert_eq!(iter.next(), Some(Edition::GtCoreBox));
    /// assert_eq!(iter.tiles(), &[tile!(101), tile!(103, a, 1)][..]);
    /// assert_eq!(iter.next(), Some(Edition::GtChampionship));
    /// assert_eq!(iter.tiles(), &[tile!(124, b)][..]);
    /// assert_eq!(iter.next(), Some(Edition::GtWorldTour));
    /// assert_eq!(iter.tiles(), &[tile!(124, a)][..]);
    /// assert_eq!(iter.next(), None);
    /// assert_eq!(iter.tiles(), &[][..]);
    /// ```
    ///
    /// Remaining tiles that do not belong to any edition are listed after the
    /// iterator has completed:
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::edition::Edition;
    /// # use rekee::tile::{TileId, TileList};
    /// let tiles = vec![tile!(101), tile!(999)];
    /// let mut iter = tiles.group_by_edition();
    ///
    /// assert_eq!(iter.next(), Some(Edition::GtCoreBox));
    /// assert_eq!(iter.tiles(), &[tile!(101)][..]);
    /// assert_eq!(iter.next(), None);
    /// assert_eq!(iter.tiles(), &[tile!(999)][..]);
    /// ```
    fn group_by_edition(&self) -> GroupByEdition;

    /// Returns an iterator over the terrain surfaces that are included in the
    /// current list of tiles.
    ///
    /// The order of terrain surfaces returned by the iterator depends on the
    /// internal implementation.
    ///
    /// Upon each step of the iterator the group of tiles that belong to the
    /// current terrain is available in [`tiles`](GroupByTerrain::tiles).
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::tile::{Terrain, TileId, TileList};
    /// let tiles = vec![tile!(301, a), tile!(220, b), tile!(418, a), tile!(419, b)];
    /// let mut iter = tiles.group_by_terrain();
    ///
    /// assert_eq!(iter.next(), Some(Terrain::Asphalt));
    /// assert_eq!(iter.tiles(), &[tile!(301, a), tile!(418, a)][..]);
    /// assert_eq!(iter.next(), Some(Terrain::Gravel));
    /// assert_eq!(iter.tiles(), &[tile!(220, b), tile!(419, b)][..]);
    /// assert_eq!(iter.next(), None);
    /// ```
    ///
    /// Tiles that do not have terrain information are listed at the end,
    /// separated from filler tiles:
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::tile::{Terrain, TileId, TileList};
    /// let tiles = vec![tile!(101), tile!(999, a)];
    /// let mut iter = tiles.group_by_terrain();
    ///
    /// assert_eq!(iter.next(), Some(Terrain::None));
    /// assert_eq!(iter.tiles(), &[tile!(101)][..]);
    /// assert_eq!(iter.next(), None);
    /// assert_eq!(iter.tiles(), &[tile!(999, a)][..]);
    /// ```
    fn group_by_terrain(&self) -> GroupByTerrain;

    /// Returns an iterator over the danger levels that occur in the current
    /// list of tiles.
    ///
    /// The order of danger levels returned by the iterator depends on the
    /// internal implementation.
    ///
    /// Upon each step of the iterator the group of tiles that belong to the
    /// current danger level is available in
    /// [`tiles`](GroupByDangerLevel::tiles).
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::tile::{DangerLevel, TileId, TileList};
    /// let tiles = vec![tile!(301, a), tile!(221, b), tile!(418, a), tile!(119, b)];
    /// let mut iter = tiles.group_by_danger_level();
    ///
    /// assert_eq!(iter.next(), Some(DangerLevel::Low));
    /// assert_eq!(iter.tiles(), &[tile!(301, a), tile!(221, b)][..]);
    /// assert_eq!(iter.next(), Some(DangerLevel::Medium));
    /// assert_eq!(iter.tiles(), &[tile!(119, b)][..]);
    /// assert_eq!(iter.next(), Some(DangerLevel::High));
    /// assert_eq!(iter.tiles(), &[tile!(418, a)][..]);
    /// assert_eq!(iter.next(), None);
    /// ```
    ///
    /// Tiles that do not have terrain information are listed at the end,
    /// separated from filler tiles:
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::tile::{DangerLevel, TileId, TileList};
    /// let tiles = vec![tile!(101), tile!(999, a)];
    /// let mut iter = tiles.group_by_danger_level();
    ///
    /// assert_eq!(iter.next(), Some(DangerLevel::None));
    /// assert_eq!(iter.tiles(), &[tile!(101)][..]);
    /// assert_eq!(iter.next(), None);
    /// assert_eq!(iter.tiles(), &[tile!(999, a)][..]);
    /// ```
    fn group_by_danger_level(&self) -> GroupByDangerLevel;

    /// Returns a summary about the tiles that are necessary to build the
    /// current list of tiles.
    ///
    /// For each base tile the entry contains the number of tile instances that
    /// are included in the current list of tiles. The returned array is always
    /// sorted by tile number.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::tile::{TileId, TileList};
    /// let tiles = vec![tile!(101), tile!(103, a, 1), tile!(103, a, 2), tile!(124, a)];
    /// for row in tiles.tile_summary() {
    ///     println!("{}x tile {}", row.count, row.tile);
    /// }
    /// ```
    fn tile_summary(&self) -> Vec<TileSummary>;

    /// Returns a summary about the editions that are necessary to build the
    /// current list of tiles.
    ///
    /// For each edition the entry contains the number of edition instances
    /// (boxes) required, and the number of tiles that belong to the edition.
    ///
    /// This method is a convenience wrapper around the [`group_by_edition`]
    /// method. It merges the tile count of editions that occur mutiple times.
    /// Different from [`group_by_edition`] the returned array is always sorted
    /// by edition. If the tile list contains unknown tiles an entry with
    /// [`edition`] set to `None` is put at the end of the summary.
    ///
    /// [`group_by_edition`]: TileList::group_by_edition
    /// [`edition`]: EditionSummary::edition
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::edition::Edition;
    /// # use rekee::tile::{TileId, TileList};
    /// let tiles = vec![tile!(101), tile!(103, a, 1), tile!(124, a), tile!(124, b)];
    /// for row in tiles.edition_summary() {
    ///     let label = match row.edition {
    ///         Some(val) => val.to_string(),
    ///         None => "Unknown".to_string(),
    ///     };
    ///     println!("{}: {}x ({} tiles)", label, row.edition_count, row.tile_count);
    /// }
    /// ```
    fn edition_summary(&self) -> Vec<EditionSummary> {
        let mut summary = BTreeMap::new();
        let mut iter = self.group_by_edition();
        loop {
            let edition = iter.next();
            let tile_count = iter.tiles().len() as u32;
            if tile_count > 0 {
                let entry = summary.entry(edition).or_insert((0, 0));
                entry.0 += 1;
                entry.1 += tile_count;
            }
            if edition.is_none() {
                break;
            }
        }
        let summary: Vec<_> = summary.iter()
            .map(|(&edition, &(edition_count, tile_count))|
                EditionSummary::new(edition, edition_count, tile_count)
            )
            .collect();
        summary
    }

    /// Returns a summary about the terrain surfaces that occur within the
    /// current list of tiles.
    ///
    /// For each terrain surface the entry contains the number of matching
    /// tiles.
    ///
    /// This method is a convenience wrapper around the [`group_by_terrain`]
    /// method. Different from [`group_by_terrain`] the returned array is always
    /// sorted by terrain. If the tile list contains unknown tiles an entry with
    /// [`terrain`] set to `None` is put at the end of the summary.
    ///
    /// [`group_by_terrain`]: TileList::group_by_terrain
    /// [`terrain`]: TerrainSummary::terrain
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::tile::{TileId, TileList};
    /// let tiles = vec![tile!(301, a), tile!(220, b), tile!(418, a), tile!(419, b)];
    /// for row in tiles.terrain_summary() {
    ///     let label = match row.terrain {
    ///         Some(val) => val.to_string(),
    ///         None => "Unknown".to_string(),
    ///     };
    ///     println!("{}: {} tiles", label, row.tile_count);
    /// }
    /// ```
    fn terrain_summary(&self) -> Vec<TerrainSummary> {
        let mut summary = Vec::with_capacity(4);
        let mut iter = self.group_by_terrain();
        let mut tile_count;
        loop {
            let terrain = iter.next();
            tile_count = iter.tiles().len() as u32;
            if terrain.is_none() {
                break;
            } else if tile_count > 0 {
                summary.push(TerrainSummary::new(terrain, tile_count));
            }
        }
        summary.sort_unstable_by_key(|item| item.terrain);
        // insert tile count for unknown tiles at the end
        if tile_count > 0 {
            summary.push(TerrainSummary::new(None, tile_count));
        }
        summary
    }

    /// Returns a summary about the danger levels that occur within the current
    /// list of tiles.
    ///
    /// For each danger level the entry contains the number of matching tiles.
    ///
    /// This method is a convenience wrapper around the
    /// [`group_by_danger_level`] method. Different from
    /// [`group_by_danger_level`] the returned array is always sorted by level
    /// value. If the tile list contains unknown tiles an entry with
    /// [`danger_level`] set to `None` is put at the end of the summary.
    ///
    /// [`group_by_danger_level`]: TileList::group_by_danger_level
    /// [`danger_level`]: DangerLevelSummary::danger_level
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::tile::{TileId, TileList};
    /// let tiles = vec![tile!(301, a), tile!(220, b), tile!(418, a), tile!(419, b)];
    /// for row in tiles.danger_level_summary() {
    ///     let label = match row.danger_level {
    ///         Some(val) => val.to_string(),
    ///         None => "Unknown".to_string(),
    ///     };
    ///     println!("{}: {} tiles", label, row.tile_count);
    /// }
    /// ```
    fn danger_level_summary(&self) -> Vec<DangerLevelSummary> {
        let mut summary = Vec::with_capacity(4);
        let mut iter = self.group_by_danger_level();
        let mut tile_count;
        loop {
            let danger_level = iter.next();
            tile_count = iter.tiles().len() as u32;
            if danger_level.is_none() {
                break;
            } else if tile_count > 0 {
                summary.push(DangerLevelSummary::new(danger_level, tile_count));
            }
        }
        summary.sort_unstable_by_key(|item| item.danger_level);
        // insert tile count for unknown tiles at the end
        if tile_count > 0 {
            summary.push(DangerLevelSummary::new(None, tile_count));
        }
        summary
    }

    /// Detects whether the given list of tiles belongs to the Rallyman GT or
    /// DIRT series.
    ///
    /// Start/Finish tiles are considered first when detecting the game series.
    /// When no such tiles are found the tiles are checked whether they all
    /// belong to the same series.
    ///
    /// Returns `Some(Series)` when detection was successful, otherwise returns
    /// `None`.
    fn detect_series(&self) -> Option<Series>;
}

impl<T: AsRef<TileId> + Clone> TileList for [T] {
    fn group_by_edition(&self) -> GroupByEdition {
        GroupByEdition::new(self)
    }

    fn group_by_terrain(&self) -> GroupByTerrain {
        GroupByTerrain::new(self)
    }

    fn group_by_danger_level(&self) -> GroupByDangerLevel {
        GroupByDangerLevel::new(self)
    }

    fn tile_summary(&self) -> Vec<TileSummary> {
        let mut summary = BTreeMap::new();
        for tile in self.iter() {
            let base_tile = tile.as_ref().base();
            let entry = summary.entry(base_tile).or_insert(0);
            *entry += 1;
        }
        let summary: Vec<_> = summary.iter()
            .map(|(&tile, &count)|
                TileSummary::new(tile, count)
            )
            .collect();
        summary
    }

    fn detect_series(&self) -> Option<Series> {
        const START_FINISH_TILES: [TileId; 19] = [
            // Rallyman GT
            tile!(102, a), tile!(102, b),
            tile!(131, a), tile!(131, b),
            tile!(139, a), tile!(139, b),
            tile!(140, b),
            tile!(143, b),
            tile!(144, b),
            // Rallyman DIRT
            tile!(202, a), tile!(202, b),
            tile!(203, a), tile!(203, b),
            tile!(301, a), tile!(301, b),
            tile!(302, a), tile!(302, b),
            tile!(401, a), tile!(401, b),
        ];
        let mut start_series = HashSet::new();
        let mut tile_series = HashSet::new();
        for tile in self.iter() {
            let tile = tile.as_ref();
            let info = match TileInfo::get(*tile) {
                Some(val) => val,
                None => continue,
            };
            let series = info.series();
            if START_FINISH_TILES.contains(tile) {
                start_series.insert(series);
            }
            tile_series.insert(series);
        }
        if start_series.len() == 1 {
            // At least one start/finish tile was used and all of them are from the same series
            start_series.into_iter().next()
        } else if tile_series.len() == 1 {
            // At least one tile was used and all of them are from the same series
            tile_series.into_iter().next()
        } else {
            None
        }
    }
}

//----------------------------------------------------------------------------

/// Tile connection hint. Used to describe the orientation of adjacent tiles
/// along the track in [`Map::append()`](crate::map::Map::append).
///
/// Represents the track characteristics of a single tile's edge-to-edge
/// connection, simplified to one of three possible values: Straight, Left, or
/// Right.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConnectionHint {
    Straight,
    Left,
    Right,
}

impl fmt::Display for ConnectionHint {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConnectionHint::Straight =>
                write!(fmt, "S")?,
            ConnectionHint::Left =>
                write!(fmt, "L")?,
            ConnectionHint::Right =>
                write!(fmt, "R")?,
        }
        Ok(())
    }
}

impl FromStr for ConnectionHint {
    type Err = ParseHintError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "S" | "s" | "" =>
                Ok(ConnectionHint::Straight),
            "L" | "l" =>
                Ok(ConnectionHint::Left),
            "R" | "r" =>
                Ok(ConnectionHint::Right),
            _ =>
                Err(ParseHintError::Unknown(s.to_string())),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseHintError {
    Unknown(String),
}

impl fmt::Display for ParseHintError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseHintError::Unknown(val) =>
                write!(fmt, "Unknown hint token \"{}\"", val),
        }
    }
}

//----------------------------------------------------------------------------

/// Internal tile connection information. Describes how a specific tile edge is
/// connected to other edges of the same tile.
///
/// This is basically a more detailed version of [`ConnectionHint`] information,
/// use [`TileInfo::connection()`] to get the connection data of a specific tile.
///
/// The connection entry contains a value that indicates the direction:
///  * `0`: edge is connected to the opposite edge of the tile (A → D, B → E, C → F)
///  * `1`: edge is connected one edge to the right of the opposite edge (A → E, B → F, C → A)
///  * `2`: edge is connected two edges to the right of the opposite edge (A → F, B → A, C → B)
///  * `-1`: edge is connected one edge to the left of the opposite edge (A → C, B → D, C → E)
///  * `-2`: edge is connected two edges to the left of the opposite edge (A → B, B → C, C → D)
///
/// Junctions work the same way, except they contain two direction values. Some
/// functions that process a single direction value only will use the first value
/// of a junction.
///
/// # Examples
///
/// ```
/// # use rekee::tile::{Connection, ConnectionHint};
/// let conn = Connection::Left(1);
/// assert!(conn == ConnectionHint::Left);
/// assert!(conn != ConnectionHint::Right);
/// ```
///
/// ```
/// # use rekee::hexagon::Direction;
/// # use rekee::tile::Connection;
/// let conn = Connection::Left(1);
/// assert_eq!(conn.target(Direction::A), Some(Direction::C));
///
/// let conn = Connection::Right(2);
/// assert_eq!(conn.target(Direction::A), Some(Direction::F));
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Connection {
    None,
    Straight(i8),
    Left(i8),
    Right(i8),
    Junction(i8, i8),
}

impl Connection {
    pub fn target(&self, source: Direction) -> Option<Direction> {
        match *self {
            Connection::None =>
                None,
            Connection::Straight(val) =>
                Some(source + (3 + val).into()),
            Connection::Left(val) =>
                Some(source + (3 - val).into()),
            Connection::Right(val) =>
                Some(source + (3 + val).into()),
            Connection::Junction(val, _) =>
                Some(source + (3 + val).into()),
        }
    }
}

impl Default for Connection {
    fn default() -> Self {
        Connection::None
    }
}

impl PartialEq<ConnectionHint> for Connection {
    fn eq(&self, other: &ConnectionHint) -> bool {
        let hint = match *self {
            Connection::None =>
                return false,
            Connection::Straight(_) =>
                ConnectionHint::Straight,
            Connection::Left(_) =>
                ConnectionHint::Left,
            Connection::Right(_) =>
                ConnectionHint::Right,
            Connection::Junction(val1, val2) => {
                if val1 > 0 {
                    ConnectionHint::Right
                } else if val1 < 0 {
                    ConnectionHint::Left
                } else if val2 > 0 {
                    ConnectionHint::Right
                } else if val2 < 0 {
                    ConnectionHint::Left
                } else {
                    ConnectionHint::Straight
                }
            },
        };
        hint == *other
    }
}

//----------------------------------------------------------------------------

/// External tile edge information. Describes how the track spaces of the edge
/// are connected to adjacent tiles.
///
/// Most tiles end with a full straight line between spaces. Some tiles from
/// Rallyman GT extensions have spaces that continue on the adjacent tile, they
/// end with a skewed half of a track space.
///
/// Additional to the space type edges have a lane count value.
///
/// Use [`TileInfo::edge()`] to get the edge data of a specific tile.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Edge {
    None,
    Straight(u8),
    SkewLeft(u8),
    SkewRight(u8),
}

impl Edge {
    /// Lane count of the tile edge.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::tile::Edge;
    /// let edge = Edge::Straight(2);
    /// assert_eq!(edge.lanes(), 2);
    /// ```
    pub fn lanes(&self) -> u8 {
        match *self {
            Edge::None => 0,
            Edge::Straight(val) => val,
            Edge::SkewLeft(val) => val,
            Edge::SkewRight(val) => val,
        }
    }
}

impl Default for Edge {
    fn default() -> Self {
        Edge::None
    }
}

//----------------------------------------------------------------------------

/// Danger level information for a tile.
///
/// Use [`TileInfo::danger_level()`] to get the danger level data of a specific tile.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "lowercase", try_from = "&str")]
pub enum DangerLevel {
    /// Tile has no danger level assigned.
    None,
    /// Tile has danger level low (yellow).
    Low,
    /// Tile has danger level medium (orange).
    Medium,
    /// Tile has danger level high (red).
    High,
}

impl Default for DangerLevel {
    fn default() -> Self {
        DangerLevel::None
    }
}

impl fmt::Display for DangerLevel {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DangerLevel::None =>
                write!(fmt, "None"),
            DangerLevel::Low =>
                write!(fmt, "Low"),
            DangerLevel::Medium =>
                write!(fmt, "Medium"),
            DangerLevel::High =>
                write!(fmt, "High"),
        }
    }
}

// small hack to provide the Serde string serialization as a formatter
impl fmt::LowerHex for DangerLevel {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut text = serde_json::to_string(self).unwrap();
        text.retain(|ch| ch != '"');
        write!(fmt, "{}", text)
    }
}

impl FromStr for DangerLevel {
    type Err = ParseDangerLevelError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.eq_ignore_ascii_case("none") {
            Ok(DangerLevel::None)
        } else if s.eq_ignore_ascii_case("low") {
            Ok(DangerLevel::Low)
        } else if s.eq_ignore_ascii_case("medium") {
            Ok(DangerLevel::Medium)
        } else if s.eq_ignore_ascii_case("high") {
            Ok(DangerLevel::High)
        } else {
            Err(ParseDangerLevelError::UnknownDangerLevel(s.to_string()))
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseDangerLevelError {
    UnknownDangerLevel(String),
}

impl fmt::Display for ParseDangerLevelError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseDangerLevelError::UnknownDangerLevel(val) =>
                write!(fmt, "Unknown danger level \"{}\"", val),
        }
    }
}

impl std::convert::TryFrom<&str> for DangerLevel {
    type Error = ParseDangerLevelError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        DangerLevel::from_str(value)
    }
}

//----------------------------------------------------------------------------

/// Terrain surface information for a tile.
///
/// Use [`TileInfo::terrain()`] to get the terrain surface data of a specific tile.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Deserialize)]
#[serde(try_from = "&str")]
pub enum Terrain {
    /// Tile has no terrain surface assigned.
    None,
    /// Tile has asphalt terrain surface.
    Asphalt,
    /// Tile has gravel terrain surface.
    Gravel,
    /// Tile has snow terrain surface.
    Snow,
}

impl Default for Terrain {
    fn default() -> Self {
        Terrain::None
    }
}

impl fmt::Display for Terrain {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Terrain::None =>
                write!(fmt, "None"),
            Terrain::Asphalt =>
                write!(fmt, "Asphalt"),
            Terrain::Gravel =>
                write!(fmt, "Gravel"),
            Terrain::Snow =>
                write!(fmt, "Snow"),
        }
    }
}

// small hack to provide the Serde string serialization as a formatter
impl fmt::LowerHex for Terrain {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut text = serde_json::to_string(self).unwrap();
        text.retain(|ch| ch != '"');
        write!(fmt, "{}", text)
    }
}

impl FromStr for Terrain {
    type Err = ParseTerrainError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.eq_ignore_ascii_case("none") {
            Ok(Terrain::None)
        } else if s.eq_ignore_ascii_case("asphalt") {
            Ok(Terrain::Asphalt)
        } else if s.eq_ignore_ascii_case("gravel") {
            Ok(Terrain::Gravel)
        } else if s.eq_ignore_ascii_case("snow") {
            Ok(Terrain::Snow)
        } else {
            Err(ParseTerrainError::UnknownTerrain(s.to_string()))
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseTerrainError {
    UnknownTerrain(String),
}

impl fmt::Display for ParseTerrainError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseTerrainError::UnknownTerrain(val) =>
                write!(fmt, "Unknown terrain name \"{}\"", val),
        }
    }
}

impl std::convert::TryFrom<&str> for Terrain {
    type Error = ParseTerrainError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Terrain::from_str(value)
    }
}

impl Serialize for Terrain {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer
    {
        let mut data = self.to_string();
        data.make_ascii_lowercase();
        serializer.serialize_str(&data)
    }
}

//----------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Pacenote {
    /// Track space with speed limit.
    Limit(u8),
    /// Track space with speed limit and hazard.
    LimitHazard(u8),
    /// Track space with automatic hazard.
    Hazard,
    /// Shortcut track space with speed limit.
    Shortcut(u8),
    /// Track space with a jump.
    Jump(u8),
    /// Track space with water.
    Water,
    /// Track spaces connected by ascend or descend arrows.
    Climb,
    /// Track space with chicane.
    Chicane,
    /// Tile with cloud modifier token.
    Cloud,
    /// Tile with oxygen modifier token.
    Oxygen(u8),
    /// Tile with shortcut mud spray token.
    MudSpray,
}

impl fmt::Display for Pacenote {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pacenote::Limit(val) =>
                write!(fmt, "{}", val),
            Pacenote::LimitHazard(val) =>
                write!(fmt, "{}!", val),
            Pacenote::Hazard =>
                write!(fmt, "Hazard"),
            Pacenote::Shortcut(val) =>
                write!(fmt, "Cut {}", val),
            Pacenote::Jump(val) =>
                write!(fmt, "Jump {}", val),
            Pacenote::Water =>
                write!(fmt, "Water"),
            Pacenote::Climb =>
                write!(fmt, "Climb"),
            Pacenote::Chicane =>
                write!(fmt, "Chicane"),
            Pacenote::Cloud =>
                write!(fmt, "Cloud"),
            Pacenote::Oxygen(val) =>
                write!(fmt, "Oxygen -{}", val),
            Pacenote::MudSpray =>
                write!(fmt, "Mud"),
        }
    }
}

//----------------------------------------------------------------------------

/// Information about tile characteristics like graphical variants, connections,
/// and edge types.
///
/// This tile characteristics data is used by [`Map`](crate::map::Map) to
/// determine the best tile direction and next active position when adding a
/// tile.
///
/// Use [`TileInfo::get()`] to lookup tile information by identifier. Note that
/// the list of tiles is static, it is not possible to add or change any tile
/// information entry at runtime.
///
/// # Examples
///
/// ```
/// # #[macro_use] extern crate rekee;
/// # use rekee::tile::{TileId, TileInfo};
/// # fn main() {
/// let info = TileInfo::get(tile!(103, a, 2));
/// assert!(info.is_some());
/// # }
/// ```
#[derive(Debug, Default)]
pub struct TileInfo {
    id: TileId,
    count: usize,
    terrain: Terrain,
    danger_level: DangerLevel,
    connections: [Connection; 6],
    edges: [Edge; 6],
    pacenotes: &'static[Pacenote],
}

impl TileInfo {
    const fn new(id: TileId, count: usize, terrain: Terrain, danger_level: DangerLevel, connections: [Connection; 6], edges: [Edge; 6], pacenotes: &'static[Pacenote]) -> Self {
        TileInfo { id, count, terrain, danger_level, connections, edges, pacenotes }
    }

    /// Base identifier of the corresponding game tile.
    ///
    /// See [`TileId::base()`] for more information on the difference between
    /// base and full tile identifiers.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::tile::{TileId, TileInfo};
    /// # fn main() {
    /// let info = TileInfo::get(tile!(103, a, 2)).unwrap();
    /// assert_eq!(info.base_id(), TileId::new(103, 1, 0));
    /// # }
    /// ```
    pub fn base_id(&self) -> TileId {
        self.id
    }

    /// Full identifier of the corresponding game tile. If the tile has multiple
    /// graphical variants, this will return the first possible variant.
    ///
    /// See [`TileId::base()`] for more information on the difference between
    /// base and full tile identifiers.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::tile::{TileId, TileInfo};
    /// # fn main() {
    /// let info = TileInfo::get(tile!(103, b)).unwrap();
    /// assert_eq!(info.full_id(), TileId::new(103, 2, 1));
    /// # }
    /// ```
    pub fn full_id(&self) -> TileId {
        match self.count {
            1 => TileId::new(self.id.num, self.id.side, 0),
            _ => TileId::new(self.id.num, self.id.side, 1),
        }
    }

    /// Returns an interator over all tiles in the internal list.
    pub fn iter() -> std::slice::Iter<'static, Self> {
        TILE_INFOS.iter()
    }

    /// Lookup tile information by tile identifier. The identifier can be in base
    /// or full format (with or without graphical variant).
    pub fn get(id: TileId) -> Option<&'static Self> {
        // Use of the fast binary search algorithm is allowed here as the proper
        // list order is checked with unit tests
        let idx = TILE_INFOS.binary_search_by_key(&id.base(), |info| info.id);
        match idx {
            Ok(idx) => Some(&TILE_INFOS[idx]),
            Err(_) => None
        }
    }

    /// Number of graphical variants that are available for a tile.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::tile::TileInfo;
    /// # fn main() {
    /// let info = TileInfo::get(tile!(103, b)).unwrap();
    /// assert_eq!(info.count(), 3);
    /// # }
    /// ```
    pub fn count(&self) -> usize {
        self.count
    }

    /// Terrain information for a tile.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::tile::{Terrain, TileInfo};
    /// # fn main() {
    /// let info = TileInfo::get(tile!(103, a)).unwrap();
    /// assert_eq!(info.terrain(), Terrain::Asphalt);
    ///
    /// let info = TileInfo::get(tile!(205, b)).unwrap();
    /// assert_eq!(info.terrain(), Terrain::Gravel);
    /// # }
    /// ```
    pub fn terrain(&self) -> Terrain {
        self.terrain
    }

    /// Danger level information for a tile.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::tile::{DangerLevel, TileInfo};
    /// # fn main() {
    /// let info = TileInfo::get(tile!(103, a)).unwrap();
    /// assert_eq!(info.danger_level(), DangerLevel::Low);
    ///
    /// let info = TileInfo::get(tile!(205, b)).unwrap();
    /// assert_eq!(info.danger_level(), DangerLevel::Medium);
    /// # }
    /// ```
    pub fn danger_level(&self) -> DangerLevel {
        self.danger_level
    }

    /// Pacenote information for a tile.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::tile::{Pacenote, TileInfo};
    /// # fn main() {
    /// let info = TileInfo::get(tile!(107, a)).unwrap();
    /// assert_eq!(info.pacenotes(), vec![Pacenote::Limit(1), Pacenote::Hazard]);
    ///
    /// let info = TileInfo::get(tile!(205, b)).unwrap();
    /// assert_eq!(info.pacenotes(), vec![Pacenote::Jump(4)]);
    /// # }
    /// ```
    pub fn pacenotes(&self) -> Vec<Pacenote> {
        self.pacenotes.to_vec()
    }

    /// Connection information for one of the six directions of a tile.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::hexagon::Direction;
    /// # use rekee::tile::{Connection, TileInfo};
    /// # fn main() {
    /// let info = TileInfo::get(tile!(103, b)).unwrap();
    /// assert_eq!(info.connection(Direction::D), Connection::Straight(0));
    ///
    /// let info = TileInfo::get(tile!(105, a)).unwrap();
    /// assert_eq!(info.connection(Direction::D), Connection::Right(1));
    /// # }
    /// ```
    pub fn connection(&self, dir: Direction) -> Connection {
        self.connections[dir]
    }

    /// Connection target direction for one of the six directions of a tile.
    ///
    /// Follows the inner connection of a tile from the given `source` direction
    /// to the target. If the connection contains a [`Connection::Junction`] the
    /// first of both target directions is returned. If the connection does not
    /// contain a connection, `None` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::hexagon::Direction;
    /// # use rekee::tile::{Connection, TileInfo};
    /// # fn main() {
    /// let info = TileInfo::get(tile!(103, b)).unwrap();
    /// assert_eq!(info.connection_target(Direction::D), Some(Direction::A));
    ///
    /// let info = TileInfo::get(tile!(105, a)).unwrap();
    /// assert_eq!(info.connection_target(Direction::D), Some(Direction::B));
    /// # }
    /// ```
    pub fn connection_target(&self, source: Direction) -> Option<Direction> {
        self.connections[source].target(source)
    }

    /// Edge informantion for one of the six directions of a tile.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::hexagon::Direction;
    /// # use rekee::tile::{Edge, TileInfo};
    /// # fn main() {
    /// let info = TileInfo::get(tile!(103, b)).unwrap();
    /// assert_eq!(info.edge(Direction::D), Edge::Straight(2));
    ///
    /// let info = TileInfo::get(tile!(105, a)).unwrap();
    /// assert_eq!(info.edge(Direction::D), Edge::Straight(3));
    /// # }
    /// ```
    pub fn edge(&self, dir: Direction) -> Edge {
        self.edges[dir]
    }

    /// Series information for a tile.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::edition::Series;
    /// # use rekee::tile::TileInfo;
    /// # fn main() {
    /// let info = TileInfo::get(tile!(103, a)).unwrap();
    /// assert_eq!(info.series(), Series::Gt);
    ///
    /// let info = TileInfo::get(tile!(205, b)).unwrap();
    /// assert_eq!(info.series(), Series::Dirt);
    /// # }
    /// ```
    pub fn series(&self) -> Series {
        if matches!(self.id.num, 100..=199) || self.id.num == 901 {
            return Series::Gt;
        }
        if matches!(self.id.num, 200..=499) || matches!(self.id.num, 902..=905) {
            return Series::Dirt;
        }
        unimplemented!();
    }

    /// List of editions which contain this tile.
    ///
    /// Ignores the graphical tile variant as `TileInfo` uses identifiers in
    /// [`TileId::base()`] format. The returned list will contain a single
    /// edition entry for most tiles, but an additional entry for some rare
    /// cases.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::edition::Edition;
    /// # use rekee::tile::TileInfo;
    /// # fn main() {
    /// let info = TileInfo::get(tile!(103, a)).unwrap();
    /// assert_eq!(info.editions(), vec![Edition::GtCoreBox]);
    ///
    /// let info = TileInfo::get(tile!(124, b, 1)).unwrap();
    /// assert_eq!(info.editions(), vec![Edition::GtChampionship, Edition::GtWorldTour]);
    /// # }
    /// ```
    pub fn editions(&self) -> Vec<Edition> {
        Edition::iter()
            .filter(|edition| edition.contains_base_tile(self.id))
            .copied()
            .collect()
    }
}

impl fmt::Display for TileInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.id)
    }
}

//----------------------------------------------------------------------------

const TN: Terrain = Terrain::None;
const TA: Terrain = Terrain::Asphalt;
const TG: Terrain = Terrain::Gravel;
const TS: Terrain = Terrain::Snow;

const DN: DangerLevel = DangerLevel::None;
const DL: DangerLevel = DangerLevel::Low;
const DM: DangerLevel = DangerLevel::Medium;
const DH: DangerLevel = DangerLevel::High;

const CN: Connection = Connection::None;
const CS0: Connection = Connection::Straight(0);
const CS1P: Connection = Connection::Straight(1);
const CS1M: Connection = Connection::Straight(-1);
const CS3: Connection = Connection::Straight(3);
const CL0: Connection = Connection::Left(0);
const CL1: Connection = Connection::Left(1);
const CL2: Connection = Connection::Left(2);
const CR0: Connection = Connection::Right(0);
const CR1: Connection = Connection::Right(1);
const CR2: Connection = Connection::Right(2);
const CJS0L1: Connection = Connection::Junction(0, -1);
const CJS0L2: Connection = Connection::Junction(0, -2);
const CJS0R1: Connection = Connection::Junction(0, 1);
const CJS0R2: Connection = Connection::Junction(0, 2);
const CJL1L2: Connection = Connection::Junction(-1, -2);
const CJL1R1: Connection = Connection::Junction(-1, 1);
const CJL1R2: Connection = Connection::Junction(-1, 2);
const CJL2R2: Connection = Connection::Junction(-2, 2);
const CJR1L2: Connection = Connection::Junction(1, -2);
const CJR1R2: Connection = Connection::Junction(1, 2);
const CJR2L2: Connection = Connection::Junction(2, -2);

const EN: Edge = Edge::None;
const ES2: Edge = Edge::Straight(2);
const ES3: Edge = Edge::Straight(3);
const ES4: Edge = Edge::Straight(4);
const EL2: Edge = Edge::SkewLeft(2);
const EL3: Edge = Edge::SkewLeft(3);
const ER2: Edge = Edge::SkewRight(2);
const ER3: Edge = Edge::SkewRight(3);

const NL1: Pacenote = Pacenote::Limit(1);
const NL2: Pacenote = Pacenote::Limit(2);
const NL3: Pacenote = Pacenote::Limit(3);
const NL4: Pacenote = Pacenote::Limit(4);
const NL5: Pacenote = Pacenote::Limit(5);
const NLH4: Pacenote = Pacenote::LimitHazard(4);
const NLH5: Pacenote = Pacenote::LimitHazard(5);
const NH: Pacenote = Pacenote::Hazard;
const NS2: Pacenote = Pacenote::Shortcut(2);
const NS3: Pacenote = Pacenote::Shortcut(3);
const NS4: Pacenote = Pacenote::Shortcut(4);
const NS5: Pacenote = Pacenote::Shortcut(5);
const NS6: Pacenote = Pacenote::Shortcut(6);
const NJ3: Pacenote = Pacenote::Jump(3);
const NJ4: Pacenote = Pacenote::Jump(4);
const NJ5: Pacenote = Pacenote::Jump(5);
const NW: Pacenote = Pacenote::Water;
const NC: Pacenote = Pacenote::Climb;

static TILE_INFOS: [TileInfo; 271] = [
    // Rallyman GT core box
    TileInfo::new(tile!(101), 1, TN, DN, [CN; 6], [EN; 6], &[]),
    TileInfo::new(tile!(102, a), 1, TA, DL, [CS0, CN, CN, CS0, CN, CN], [ES3, EN, EN, ES3, EN, EN], &[]),
    TileInfo::new(tile!(102, b), 1, TA, DM, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(103, a), 3, TA, DL, [CS0, CN, CN, CS0, CN, CN], [ES3, EN, EN, ES3, EN, EN], &[]),
    TileInfo::new(tile!(103, b), 3, TA, DM, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(104, a), 3, TA, DM, [CS0, CN, CN, CS0, CN, CN], [ES3, EN, EN, ES3, EN, EN], &[]),
    TileInfo::new(tile!(104, b), 3, TA, DH, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(105, a), 2, TA, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES3, EN, ES3, EN, EN], &[]),
    TileInfo::new(tile!(105, b), 2, TA, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(106, a), 2, TA, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES3, EN, ES3, EN, EN], &[]),
    TileInfo::new(tile!(106, b), 2, TA, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(107, a), 2, TA, DL, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES3, ES3, EN, EN], &[NL1, NH]),
    TileInfo::new(tile!(107, b), 2, TA, DM, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NH]),
    TileInfo::new(tile!(108, a), 1, TA, DL, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES3, ES3, EN, EN], &[NL1, NH]),
    TileInfo::new(tile!(108, b), 1, TA, DM, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NH]),
    TileInfo::new(tile!(109, a), 1, TA, DM, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES3, ES3, EN, EN], &[NL1, NH]),
    TileInfo::new(tile!(109, b), 1, TA, DH, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1]),
    TileInfo::new(tile!(110, a), 1, TA, DL, [CL0, CN, CN, CR0, CN, CN], [ES3, EN, EN, ES3, EN, EN], &[NL1]),
    TileInfo::new(tile!(110, b), 1, TA, DM, [CL0, CN, CN, CR0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NL1]),
    TileInfo::new(tile!(111, a), 2, TA, DL, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES3, ES3, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(111, b), 2, TA, DM, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(112, a), 2, TA, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES3, EN, ES3, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(112, b), 2, TA, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(113, a), 1, TA, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES3, EN, ES3, EN, EN], &[NL2, NH]),
    TileInfo::new(tile!(113, b), 1, TA, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL2, NH]),
    TileInfo::new(tile!(114, a), 1, TA, DL, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES3, ES3, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(114, b), 1, TA, DM, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL2]),
    TileInfo::new(tile!(115, a), 2, TA, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES3, EN, ES3, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(115, b), 2, TA, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(116, a), 2, TA, DM, [CL0, CN, CN, CR0, CN, CN], [ES3, EN, EN, ES3, EN, EN], &[NL1, NL2, NL3]),
    TileInfo::new(tile!(116, b), 2, TA, DH, [CL0, CN, CN, CR0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NL1, NL3]),
    TileInfo::new(tile!(117, a), 2, TA, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES3, EN, ES3, EN, EN], &[NL3, NL4, NLH4]),
    TileInfo::new(tile!(117, b), 2, TA, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL3, NL4, NLH4]),
    TileInfo::new(tile!(118, a), 1, TA, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES3, EN, ES3, EN, EN], &[NL4, NL5, NLH5]),
    TileInfo::new(tile!(118, b), 1, TA, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL4, NL5, NLH5]),
    TileInfo::new(tile!(119, a), 1, TA, DL, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES3, EN, EN], &[]),
    TileInfo::new(tile!(119, b), 1, TA, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES3, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(120, a), 1, TA, DL, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES3, EN, EN], &[]),
    TileInfo::new(tile!(120, b), 1, TA, DM, [CN, CN, CN, CL1, CN, CR1], [EN, EN, EN, ES3, EN, ES2], &[NL2, NL3]),
    // Rallyman GT expansions
    TileInfo::new(tile!(121, a), 2, TA, DL, [CN, CN, CS1M, CN, CS1P, CN], [EN, EN, EL3, EN, ER3, EN], &[]),
    TileInfo::new(tile!(121, b), 2, TA, DM, [CN, CN, CS1M, CN, CS1P, CN], [EN, EN, EL2, EN, ER2, EN], &[]),
    TileInfo::new(tile!(122, a), 2, TA, DL, [CN, CN, CN, CN, CL2, CR2], [EN, EN, EN, EN, ER3, ES3], &[NL1, NH]),
    TileInfo::new(tile!(122, b), 2, TA, DM, [CN, CN, CN, CN, CL2, CR2], [EN, EN, EN, EN, ER2, ES2], &[NL1, NH]),
    TileInfo::new(tile!(123, a), 1, TA, DL, [CN, CL2, CR2, CN, CN, CN], [EN, ES3, EL3, EN, EN, EN], &[NL1, NH]),
    TileInfo::new(tile!(123, b), 1, TA, DM, [CN, CL2, CR2, CN, CN, CN], [EN, ES2, EL2, EN, EN, EN], &[NL1, NH]),
    TileInfo::new(tile!(124, a), 2, TA, DM, [CL1, CN, CR1, CN, CN, CN], [ES3, EN, EL3, EN, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(124, b), 2, TA, DH, [CL1, CN, CR1, CN, CN, CN], [ES2, EN, EL2, EN, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(125, a), 2, TA, DL, [CR1, CN, CN, CN, CL1, CN], [ES3, EN, EN, EN, ER3, EN], &[NL1, NL2]),
    TileInfo::new(tile!(125, b), 2, TA, DM, [CR1, CN, CN, CN, CL1, CN], [ES2, EN, EN, EN, ER2, EN], &[NL1, NL2]),
    TileInfo::new(tile!(126, a), 1, TA, DM, [CN, CN, CN, CL2, CR2, CN], [EN, EN, EN, ES3, ER3, EN], &[NL1, NL2]),
    TileInfo::new(tile!(126, b), 1, TA, DH, [CN, CN, CN, CL2, CR2, CN], [EN, EN, EN, ES2, ER2, EN], &[NL1, NL2]),
    TileInfo::new(tile!(127, a), 2, TA, DL, [CN, CN, CL2, CR2, CN, CN], [EN, EN, EL3, ES3, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(127, b), 2, TA, DM, [CN, CN, CL2, CR2, CN, CN], [EN, EN, EL2, ES2, EN, EN], &[NL2, NH]),
    TileInfo::new(tile!(128, a), 1, TA, DM, [CN, CN, CN, CN, CL2, CR2], [EN, EN, EN, EN, ER3, EL3], &[NL1, NL2, NL3]),
    TileInfo::new(tile!(128, b), 1, TA, DH, [CN, CN, CN, CN, CL2, CR2], [EN, EN, EN, EN, ER2, EL2], &[NL2, NL3]),
    TileInfo::new(tile!(129, a), 1, TA, DL, [CN, CR0, CN, CN, CL0, CN], [EN, ES3, EN, EN, ER3, EN], &[NL3, NL4, NLH4]),
    TileInfo::new(tile!(129, b), 1, TA, DM, [CN, CR0, CN, CN, CL0, CN], [EN, ES2, EN, EN, ER2, EN], &[NL3, NL4]),
    TileInfo::new(tile!(130, a), 1, TA, DL, [CN, CN, CR0, CN, CN, CL0], [EN, EN, EL3, EN, EN, ES3], &[NL3, NL4, NLH4]),
    TileInfo::new(tile!(130, b), 1, TA, DM, [CN, CN, CR0, CN, CN, CL0], [EN, EN, EL2, EN, EN, ES2], &[NL3, NL4]),
    TileInfo::new(tile!(131, a), 1, TA, DL, [CN, CN, CS1M, CN, CS1P, CN], [EN, EN, EL3, EN, ER3, EN], &[]),
    TileInfo::new(tile!(131, b), 1, TA, DM, [CN, CN, CS1M, CN, CS1P, CN], [EN, EN, EL2, EN, ER2, EN], &[]),
    TileInfo::new(tile!(132, a), 1, TA, DL, [CN, CN, CS1M, CN, CS1P, CN], [EN, EN, EL3, EN, ER3, EN], &[]),
    TileInfo::new(tile!(132, b), 1, TA, DM, [CN, CN, CS1M, CN, CS1P, CN], [EN, EN, EL2, EN, ER2, EN], &[]),
    TileInfo::new(tile!(133, a), 1, TA, DM, [CN, CS1P, CS1M, CN, CS1P, CS1M], [EN, ER3, EL3, EN, ER3, EL3], &[]),
    TileInfo::new(tile!(133, b), 1, TA, DH, [CN, CS1P, CS1M, CN, CS1P, CS1M], [EN, ER2, EL2, EN, ER2, EL2], &[]),
    TileInfo::new(tile!(134, a), 1, TA, DL, [CN, CN, CR0, CN, CN, CR0], [EN, EN, EL3, EN, EN, EL3], &[NL1, NL2, NL3]),
    TileInfo::new(tile!(134, b), 1, TA, DM, [CN, CN, CR0, CN, CN, CR0], [EN, EN, EL2, EN, EN, EL2], &[NL1, NL3]),
    TileInfo::new(tile!(135, a), 1, TA, DM, [CN, CL0, CN, CN, CL0, CN], [EN, ER3, EN, EN, ER3, EN], &[NL1, NL2, NL3]),
    TileInfo::new(tile!(135, b), 1, TA, DH, [CN, CL0, CN, CN, CL0, CN], [EN, ER2, EN, EN, ER2, EN], &[NL1, NL3]),
    TileInfo::new(tile!(136, a), 1, TA, DL, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES3, ES3, EN, EN], &[NL3, NL4, NLH5]),
    TileInfo::new(tile!(136, b), 1, TA, DM, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL2, NLH5]),
    TileInfo::new(tile!(137, a), 1, TA, DM, [CN, CN, CL1, CN, CR1, CN], [EN, EN, ES3, EN, ER3, EN], &[NL4, NL5]),
    TileInfo::new(tile!(137, b), 1, TA, DH, [CN, CN, CL1, CN, CR1, CN], [EN, EN, ES2, EN, ER2, EN], &[NL4, NL5, NLH5]),
    TileInfo::new(tile!(138, a), 2, TA, DL, [CN, CN, CL1, CN, CR1, CN], [EN, EN, EL3, EN, ES3, EN], &[NL4, NL5]),
    TileInfo::new(tile!(138, b), 2, TA, DM, [CN, CN, CL1, CN, CR1, CN], [EN, EN, EL2, EN, ES2, EN], &[NL4, NL5, NLH5]),
    TileInfo::new(tile!(139, a), 1, TA, DL, [CS0, CN, CN, CS0, CN, CN], [ES4, EN, EN, ES4, EN, EN], &[]),
    TileInfo::new(tile!(139, b), 1, TA, DL, [CN, CS1P, CS1M, CN, CS1P, CS1M], [EN, ER3, EL3, EN, ER3, EL3], &[]),
    TileInfo::new(tile!(140, a), 1, TA, DL, [CS0, CN, CN, CS0, CN, CN], [ES4, EN, EN, ES4, EN, EN], &[]),
    TileInfo::new(tile!(140, b), 1, TA, DL, [CN, CS1P, CS1M, CN, CS1P, CS1M], [EN, ER3, EL3, EN, ER3, EL3], &[]),
    TileInfo::new(tile!(141, a), 1, TA, DL, [CS0, CN, CN, CS0, CN, CN], [ES4, EN, EN, ES4, EN, EN], &[]),
    TileInfo::new(tile!(141, b), 1, TA, DL, [CS0, CN, CS0, CS0, CN, CS0], [ES3, EN, ES3, ES3, EN, ES3], &[]),
    TileInfo::new(tile!(142, a), 1, TA, DL, [CS0, CN, CN, CS0, CN, CN], [ES4, EN, EN, ES4, EN, EN], &[]),
    TileInfo::new(tile!(142, b), 1, TA, DL, [CS0, CN, CS1M, CS0, CS1P, CN], [ES3, EN, EL3, ES3, ER3, EN], &[]),
    TileInfo::new(tile!(143, a), 1, TA, DM, [CS0, CN, CN, CS0, CN, CN], [ES4, EN, EN, ES4, EN, EN], &[]),
    TileInfo::new(tile!(143, b), 1, TA, DM, [CN, CS1P, CS1M, CN, CS1P, CS1M], [EN, ER2, EL2, EN, ER2, EL2], &[]),
    TileInfo::new(tile!(144, a), 1, TA, DM, [CS0, CN, CN, CS0, CN, CN], [ES4, EN, EN, ES4, EN, EN], &[]),
    TileInfo::new(tile!(144, b), 1, TA, DM, [CN, CS1P, CS1M, CN, CS1P, CS1M], [EN, ER2, EL2, EN, ER2, EL2], &[]),
    TileInfo::new(tile!(145, a), 1, TA, DM, [CS0, CN, CN, CS0, CN, CN], [ES4, EN, EN, ES4, EN, EN], &[]),
    TileInfo::new(tile!(145, b), 1, TA, DH, [CS0, CN, CS0, CS0, CN, CS0], [ES2, EN, ES2, ES2, EN, ES2], &[]),
    TileInfo::new(tile!(146, a), 1, TA, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES4, EN, ES4, EN, EN], &[]),
    TileInfo::new(tile!(146, b), 1, TA, DL, [CS0, CN, CS1M, CS0, CS1P, CN], [ES2, EN, EL2, ES2, ER2, EN], &[]),
    TileInfo::new(tile!(147, a), 1, TA, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES4, EN, ES4, EN, EN], &[]),
    TileInfo::new(tile!(147, b), 1, TA, DH, [CN, CS1P, CN, CL2, CR2, CS1M], [EN, ER3, EN, ES3, ER3, EL3], &[NL1, NL2]),
    TileInfo::new(tile!(148, a), 1, TA, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES4, EN, ES4, EN, EN], &[]),
    TileInfo::new(tile!(148, b), 1, TA, DH, [CN, CS1P, CL2, CR2, CN, CS1M], [EN, ER3, EL3, ES3, EN, EL3], &[NL1, NL2]),
    TileInfo::new(tile!(149, a), 1, TA, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES4, EN, ES4, EN, EN], &[]),
    TileInfo::new(tile!(149, b), 1, TA, DM, [CN, CS1P, CL2, CR2, CN, CS1M], [EN, ER3, ES3, ES3, EN, EL3], &[NL1, NH]),
    TileInfo::new(tile!(150, a), 1, TA, DM, [CS0, CS1P, CS1M, CS0, CS1P, CS1M], [ES3, ER3, EL3, ES3, ER3, EL3], &[]),
    TileInfo::new(tile!(150, b), 1, TA, DH, [CS0, CS1P, CS1M, CS0, CS1P, CS1M], [ES2, ER2, EL2, ES2, ER2, EL2], &[]),
    TileInfo::new(tile!(151, a), 1, TA, DM, [CJS0L2, CJL1R2, CN, CJS0R1, CN, CN], [ES3, ES3, EN, ES3, EN, EN], &[NL1, NL2, NL3]),
    TileInfo::new(tile!(151, b), 1, TA, DH, [CJS0L2, CJL1R2, CN, CJS0R1, CN, CN], [ES2, ES2, EN, ES2, EN, EN], &[NL1, NL2, NL3]),
    TileInfo::new(tile!(152, a), 1, TA, DM, [CJS0R2, CN, CN, CJS0L1, CN, CJR1L2], [ES3, EN, EN, ES3, EN, ES3], &[NL1, NL2, NL3]),
    TileInfo::new(tile!(152, b), 1, TA, DH, [CJS0R2, CN, CN, CJS0L1, CN, CJR1L2], [ES2, EN, EN, ES2, EN, ES2], &[NL1, NL2, NL3]),
    TileInfo::new(tile!(153, a), 1, TA, DH, [CS0, CS0, CN, CS0, CS0, CN], [ES3, ES2, EN, ES3, ES2, EN], &[]),
    TileInfo::new(tile!(153, b), 1, TA, DH, [CS0, CN, CS0, CS0, CN, CS0], [ES2, EN, ES2, ES2, EN, ES2], &[]),
    TileInfo::new(tile!(154, a), 1, TA, DH, [CN, CS0, CN, CL1, CS0, CR1], [EN, ES2, EN, ES3, ES2, ES3], &[]),
    TileInfo::new(tile!(154, b), 1, TA, DH, [CN, CL1, CS0, CR1, CN, CS0], [EN, ES2, ES2, ES2, EN, ES2], &[]),
    // Rallyman DIRT core box
    TileInfo::new(tile!(201, a), 1, TN, DN, [CN, CN, CN, CS3, CN, CN], [EN, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(201, b), 1, TN, DN, [CN, CN, CN, CS3, CN, CN], [EN, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(202, a), 1, TG, DL, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(202, b), 1, TG, DM, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(203, a), 1, TG, DL, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(203, b), 1, TG, DH, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(204, a), 1, TG, DL, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(204, b), 1, TG, DM, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NJ3]),
    TileInfo::new(tile!(205, a), 1, TG, DL, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(205, b), 1, TG, DM, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NJ4]),
    TileInfo::new(tile!(206, a), 1, TG, DL, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(206, b), 1, TG, DL, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NJ5]),
    TileInfo::new(tile!(207, a), 1, TG, DM, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(207, b), 1, TG, DM, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NJ4, NW]),
    TileInfo::new(tile!(208, a), 1, TG, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(208, b), 1, TG, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NW]),
    TileInfo::new(tile!(209, a), 1, TG, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(209, b), 1, TG, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NJ3]),
    TileInfo::new(tile!(210, a), 1, TG, DL, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(210, b), 1, TG, DL, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(211, a), 1, TG, DM, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(211, b), 1, TG, DM, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2, NS2]),
    TileInfo::new(tile!(212, a), 1, TG, DL, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(212, b), 1, TG, DL, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2, NS2]),
    TileInfo::new(tile!(213, a), 1, TG, DH, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(213, b), 1, TG, DH, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2, NS3]),
    TileInfo::new(tile!(214, a), 1, TG, DL, [CL0, CN, CN, CR0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(214, b), 1, TG, DM, [CL0, CN, CN, CR0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NL1, NL2, NS5]),
    TileInfo::new(tile!(215, a), 1, TG, DM, [CR0, CN, CN, CR0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(215, b), 1, TG, DL, [CR0, CN, CN, CR0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NL1, NL2, NS3]),
    TileInfo::new(tile!(216, a), 1, TG, DL, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(216, b), 1, TG, DM, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(217, a), 1, TG, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(217, b), 1, TG, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL2, NL3, NS3]),
    TileInfo::new(tile!(218, a), 1, TG, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(218, b), 1, TG, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL2, NL3, NW]),
    TileInfo::new(tile!(219, a), 1, TG, DM, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(219, b), 1, TG, DM, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL2, NL3, NS2]),
    TileInfo::new(tile!(220, a), 1, TG, DH, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(220, b), 1, TG, DH, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL2, NL3, NJ3, NW]),
    TileInfo::new(tile!(221, a), 1, TG, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(221, b), 1, TG, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL2, NL3, NS4]),
    TileInfo::new(tile!(222, a), 1, TG, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL3, NL4]),
    TileInfo::new(tile!(222, b), 1, TG, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL3, NL4, NS4]),
    TileInfo::new(tile!(223, a), 1, TG, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL3, NL4]),
    TileInfo::new(tile!(223, b), 1, TG, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL3, NL4, NS4]),
    TileInfo::new(tile!(224, a), 1, TG, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL3, NL4]),
    TileInfo::new(tile!(224, b), 1, TG, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL3, NL4, NS5]),
    TileInfo::new(tile!(225, a), 1, TG, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL3, NL4]),
    TileInfo::new(tile!(225, b), 1, TG, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL3, NL4, NW]),
    TileInfo::new(tile!(226, a), 1, TG, DH, [CL0, CN, CN, CR0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NL3, NL4]),
    TileInfo::new(tile!(226, b), 1, TG, DL, [CL0, CN, CN, CR0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NL3, NL4, NS5]),
    TileInfo::new(tile!(227, a), 1, TG, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL4, NL5]),
    TileInfo::new(tile!(227, b), 1, TG, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL4, NL5, NS5]),
    TileInfo::new(tile!(228, a), 1, TG, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL4, NL5]),
    TileInfo::new(tile!(228, b), 1, TG, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL4, NL5, NJ4]),
    TileInfo::new(tile!(229, a), 1, TG, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL4, NL5]),
    TileInfo::new(tile!(229, b), 1, TG, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL4, NL5, NW]),
    TileInfo::new(tile!(230, a), 1, TG, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL4, NL5]),
    TileInfo::new(tile!(230, b), 1, TG, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL4, NL5, NS6]),
    TileInfo::new(tile!(231, a), 1, TG, DL, [CR0, CN, CN, CR0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NL5]),
    TileInfo::new(tile!(231, b), 1, TG, DM, [CR0, CN, CN, CR0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NL4]),
    TileInfo::new(tile!(232, a), 1, TG, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(232, b), 1, TG, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL1, NL2, NS4]),
    // Rallyman DIRT 110% expansion
    TileInfo::new(tile!(301, a), 1, TA, DL, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(301, b), 1, TS, DM, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(302, a), 1, TA, DL, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(302, b), 1, TS, DM, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(303, a), 1, TA, DL, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(303, b), 1, TS, DL, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(304, a), 1, TA, DH, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(304, b), 1, TS, DH, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(305, a), 1, TA, DM, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(305, b), 1, TS, DM, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(306, a), 1, TA, DM, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NJ4]),
    TileInfo::new(tile!(306, b), 1, TS, DM, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NJ4]),
    TileInfo::new(tile!(307, a), 1, TA, DH, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NJ3]),
    TileInfo::new(tile!(307, b), 1, TS, DH, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NJ3]),
    TileInfo::new(tile!(308, a), 1, TA, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(308, b), 1, TS, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(309, a), 1, TA, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(309, b), 1, TS, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[]),
    TileInfo::new(tile!(310, a), 1, TA, DL, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(310, b), 1, TS, DL, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(311, a), 1, TA, DM, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2, NS2]),
    TileInfo::new(tile!(311, b), 1, TS, DM, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(312, a), 1, TA, DL, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(312, b), 1, TS, DL, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(313, a), 1, TA, DL, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(313, b), 1, TS, DL, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(314, a), 1, TA, DH, [CR0, CN, CN, CR0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NL1, NL2, NS3]),
    TileInfo::new(tile!(314, b), 1, TS, DH, [CR0, CN, CN, CR0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(315, a), 1, TA, DH, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(315, b), 1, TS, DH, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(316, a), 1, TA, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(316, b), 1, TS, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(317, a), 1, TA, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL2, NL3, NS3]),
    TileInfo::new(tile!(317, b), 1, TS, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(318, a), 1, TA, DH, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL2, NL3, NS3]),
    TileInfo::new(tile!(318, b), 1, TS, DH, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(319, a), 1, TA, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL3, NL4]),
    TileInfo::new(tile!(319, b), 1, TS, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL3, NL4]),
    TileInfo::new(tile!(320, a), 1, TA, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL3, NL4]),
    TileInfo::new(tile!(320, b), 1, TS, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL3, NL4]),
    TileInfo::new(tile!(321, a), 1, TA, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL3, NL4, NS4]),
    TileInfo::new(tile!(321, b), 1, TS, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL3, NL4]),
    TileInfo::new(tile!(322, a), 1, TA, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL4, NL5]),
    TileInfo::new(tile!(322, b), 1, TS, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL4, NL5]),
    TileInfo::new(tile!(323, a), 1, TA, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL4, NL5]),
    TileInfo::new(tile!(323, b), 1, TS, DL, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL4, NL5]),
    TileInfo::new(tile!(324, a), 1, TA, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL4, NL5, NS6]),
    TileInfo::new(tile!(324, b), 1, TS, DM, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NL4, NL5]),
    TileInfo::new(tile!(325, a), 1, TA, DH, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NC]),
    TileInfo::new(tile!(325, b), 1, TS, DH, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NC]),
    TileInfo::new(tile!(326, a), 1, TA, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NC]),
    TileInfo::new(tile!(326, b), 1, TS, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NC]),
    // Rallyman DIRT RX expansion
    TileInfo::new(tile!(401, a), 1, TA, DL, [CS0, CN, CN, CS0, CN, CN], [ES3, EN, EN, ES3, EN, EN], &[]),
    TileInfo::new(tile!(401, b), 1, TA, DL, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES3, EN, EN], &[]),
    TileInfo::new(tile!(402, a), 1, TA, DM, [CJS0R2, CN, CN, CJS0L1, CN, CJR1L2], [ES2, EN, EN, ES2, EN, ES2], &[NL2, NL4]),
    TileInfo::new(tile!(402, b), 1, TG, DM, [CJS0R2, CN, CN, CJS0L1, CN, CJR1L2], [ES2, EN, EN, ES2, EN, ES2], &[NL1, NL3]),
    TileInfo::new(tile!(403, a), 1, TA, DM, [CJS0R2, CN, CN, CJS0L1, CN, CJR1L2], [ES3, EN, EN, ES3, EN, ES2], &[NL1, NL4]),
    TileInfo::new(tile!(403, b), 1, TG, DM, [CJS0R2, CN, CN, CJS0L1, CN, CJR1L2], [ES2, EN, EN, ES2, EN, ES2], &[NL2, NL3]),
    TileInfo::new(tile!(404, a), 1, TA, DM, [CJS0L2, CJL1R2, CN, CJS0R1, CN, CN], [ES2, ES2, EN, ES2, EN, EN], &[NL2, NL4]),
    TileInfo::new(tile!(404, b), 1, TG, DM, [CJS0L2, CJL1R2, CN, CJS0R1, CN, CN], [ES2, ES2, EN, ES2, EN, EN], &[NL1, NL3]),
    TileInfo::new(tile!(405, a), 1, TA, DM, [CJS0L2, CJL1R2, CN, CJS0R1, CN, CN], [ES3, ES2, EN, ES3, EN, EN], &[NL1, NL4]),
    TileInfo::new(tile!(405, b), 1, TG, DM, [CJS0L2, CJL1R2, CN, CJS0R1, CN, CN], [ES2, ES2, EN, ES2, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(406, a), 1, TA, DM, [CN, CJL1L2, CJL2R2, CJR1R2, CN, CN], [EN, ES2, ES2, ES2, EN, EN], &[NL1, NL3]),
    TileInfo::new(tile!(406, b), 1, TG, DM, [CN, CJL1L2, CJL2R2, CJR1R2, CN, CN], [EN, ES2, ES2, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(407, a), 1, TA, DM, [CN, CJL1L2, CJL2R2, CJR1R2, CN, CN], [EN, ES2, ES3, ES3, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(407, b), 1, TG, DM, [CN, CJL1L2, CJL2R2, CJR1R2, CN, CN], [EN, ES2, ES2, ES2, EN, EN], &[NL1]),
    TileInfo::new(tile!(408, a), 1, TA, DM, [CN, CN, CN, CJL1L2, CJR2L2, CJR1R2], [EN, EN, EN, ES2, ES2, ES2], &[NL1, NL3]),
    TileInfo::new(tile!(408, b), 1, TG, DM, [CN, CN, CN, CJL1L2, CJR2L2, CJR1R2], [EN, EN, EN, ES2, ES2, ES2], &[NL1, NL3]),
    TileInfo::new(tile!(409, a), 1, TA, DM, [CN, CN, CN, CJL1L2, CJR2L2, CJR1R2], [EN, EN, EN, ES3, ES3, ES2], &[NL1, NL2]),
    TileInfo::new(tile!(409, b), 1, TG, DM, [CN, CN, CN, CJL1L2, CJR2L2, CJR1R2], [EN, EN, EN, ES2, ES2, ES2], &[NL1]),
    TileInfo::new(tile!(410, a), 1, TA, DM, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL1, NL2]),
    TileInfo::new(tile!(410, b), 1, TG, DM, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN], &[NL2, NL3]),
    TileInfo::new(tile!(411, a), 1, TA, DM, [CL0, CN, CN, CR0, CN, CN], [ES3, EN, EN, ES2, EN, EN], &[NL3]),
    TileInfo::new(tile!(411, b), 1, TG, DM, [CL0, CN, CN, CR0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NL3, NL4]),
    TileInfo::new(tile!(412, a), 1, TA, DM, [CN, CS0, CL2, CR2, CS0, CN], [EN, ES2, ES2, ES2, ES2, EN], &[NL2, NL3, NJ3]),
    TileInfo::new(tile!(412, b), 1, TG, DM, [CN, CS0, CL2, CR2, CS0, CN], [EN, ES2, ES2, ES2, ES2, EN], &[NL1, NL2, NJ4]),
    TileInfo::new(tile!(413, a), 1, TA, DM, [CL1, CN, CR1, CN, CL2, CR2], [ES2, EN, ES2, EN, ES2, ES2], &[NL2, NL3]),
    TileInfo::new(tile!(413, b), 1, TG, DM, [CN, CL1, CN, CR1, CL2, CR2], [EN, ES2, EN, ES2, ES2, ES2], &[NL1, NL2]),
    TileInfo::new(tile!(414, a), 1, TA, DM, [CS1M, CN, CS1P, CS1M, CN, CS1P], [ES2, EN, ES2, ES3, EN, ES3], &[NJ4, NJ5]),
    TileInfo::new(tile!(414, b), 1, TG, DM, [CS1M, CN, CS1P, CS1M, CN, CS1P], [ES2, EN, ES2, ES2, EN, ES2], &[NJ3, NJ4]),
    TileInfo::new(tile!(415, a), 1, TA, DM, [CN, CJL1R1, CN, CJL1R1, CN, CJL1R1], [EN, ES2, EN, ES3, EN, ES2], &[NL3, NL4]),
    TileInfo::new(tile!(415, b), 1, TG, DM, [CN, CJL1R1, CN, CJL1R1, CN, CJL1R1], [EN, ES2, EN, ES2, EN, ES2], &[NL3]),
    TileInfo::new(tile!(416, a), 1, TA, DH, [CS0, CN, CS0, CS0, CN, CS0], [ES2, EN, ES2, ES2, EN, ES2], &[NJ5]),
    TileInfo::new(tile!(416, b), 1, TG, DH, [CN, CS0, CS0, CN, CS0, CS0], [EN, ES2, ES2, EN, ES2, ES2], &[NJ4]),
    TileInfo::new(tile!(417, a), 1, TA, DH, [CN, CL1, CS0, CR1, CN, CS0], [EN, ES2, ES2, ES2, EN, ES2], &[NJ5]),
    TileInfo::new(tile!(417, b), 1, TG, DH, [CN, CS0, CN, CL1, CS0, CR1], [EN, ES2, EN, ES2, ES2, ES2], &[NJ4]),
    TileInfo::new(tile!(418, a), 1, TA, DH, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NC]),
    TileInfo::new(tile!(418, b), 1, TG, DH, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN], &[NC]),
    TileInfo::new(tile!(419, a), 1, TA, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NC]),
    TileInfo::new(tile!(419, b), 1, TG, DH, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN], &[NC]),
    // Unnumbered fillers
    TileInfo::new(tile!(901, a), 1, TN, DN, [CN; 6], [EN; 6], &[]), // Sheep Summer
    TileInfo::new(tile!(901, b), 1, TN, DN, [CN; 6], [EN; 6], &[]), // People City
    TileInfo::new(tile!(902, a), 1, TN, DN, [CN; 6], [EN; 6], &[]), // Barbecue Summer
    TileInfo::new(tile!(902, b), 1, TN, DN, [CN; 6], [EN; 6], &[]), // Glühwein Winter
    TileInfo::new(tile!(903, a), 1, TN, DN, [CN; 6], [EN; 6], &[]), // Lake
    TileInfo::new(tile!(903, b), 1, TN, DN, [CN; 6], [EN; 6], &[]), // Lake with Accident
    TileInfo::new(tile!(904, a), 1, TN, DN, [CN; 6], [EN; 6], &[]), // Rocks
    TileInfo::new(tile!(904, b), 1, TN, DN, [CN; 6], [EN; 6], &[]), // Sheep Winter
    TileInfo::new(tile!(905, a), 1, TN, DN, [CN; 6], [EN; 6], &[]), // Podium
    TileInfo::new(tile!(905, b), 1, TN, DN, [CN; 6], [EN; 6], &[]), // Podium
];

//----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn id_to_str() {
        assert_eq!(TileId::default().to_string(), "0");
        assert_eq!(TileId::new(101, 0, 0).to_string(), "101");
        assert_eq!(TileId::new(102, 1, 0).to_string(), "102a");
        assert_eq!(TileId::new(102, 2, 0).to_string(), "102b");
        assert_eq!(TileId::new(103, 1, 1).to_string(), "103a-1");
        assert_eq!(TileId::new(103, 2, 3).to_string(), "103b-3");
        assert_eq!(TileId::new(905, 1, 0).to_string(), "905a");

        assert_eq!(TileId::new(101, 0, 0).base().to_string(), "101");
        assert_eq!(TileId::new(101, 1, 0).base().to_string(), "101");
        assert_eq!(TileId::new(102, 1, 0).base().to_string(), "102a");
        assert_eq!(TileId::new(102, 2, 0).base().to_string(), "102b");
        assert_eq!(TileId::new(104, 1, 2).base().to_string(), "104a");
        assert_eq!(TileId::new(905, 2, 0).base().to_string(), "905b");
    }

    #[test]
    fn id_from_str() {
        assert_eq!("0".parse::<TileId>(), Ok(TileId::new(0, 0, 0)));
        assert_eq!("101".parse::<TileId>(), Ok(TileId::new(101, 0, 0)));
        assert_eq!("102a".parse::<TileId>(), Ok(TileId::new(102, 1, 0)));
        assert_eq!("102b".parse::<TileId>(), Ok(TileId::new(102, 2, 0)));
        assert_eq!("103a-1".parse::<TileId>(), Ok(TileId::new(103, 1, 1)));
        assert_eq!("103b-3".parse::<TileId>(), Ok(TileId::new(103, 2, 3)));
        assert_eq!("905A".parse::<TileId>(), Ok(TileId::new(905, 1, 0)));

        assert!("".parse::<TileId>().is_err());
        assert!("101x".parse::<TileId>().is_err());
    }

    #[test]
    fn id_serde() {
        let tile = tile!(102, a);
        let text = serde_json::to_string(&tile).unwrap();
        assert_eq!(text, r#""102a""#);

        let text = r#""103b-1""#;
        let tile: TileId = serde_json::from_str(text).unwrap();
        assert_eq!(tile, TileId::new(103, 2, 1));

        let text = r#""PODIUM""#;
        let tile: TileId = serde_json::from_str(text).unwrap();
        assert_eq!(tile, TileId::new(905, 1, 0));

        let text = r#""#;
        let result: Result<TileId, _> = serde_json::from_str(text);
        assert!(result.is_err());

        let text = r#""a-1""#;
        let result: Result<TileId, _> = serde_json::from_str(text);
        assert!(result.is_err());
    }

    #[test]
    fn tile_list_summary() {
        let tiles: &[TileId] = &[][..];
        let summary = tiles.tile_summary();
        assert_eq!(summary, vec![]);

        let tiles = [tile!(101)];
        let summary = tiles.tile_summary();
        assert_eq!(summary, vec![
            TileSummary::new(tile!(101), 1),
        ]);

        let tiles = [tile!(101), tile!(103, a, 1), tile!(103, a, 2)];
        let summary = tiles.tile_summary();
        assert_eq!(summary, vec![
            TileSummary::new(tile!(101), 1),
            TileSummary::new(tile!(103, a), 2),
        ]);
    }

    #[test]
    fn tile_list_group_by_edition() {
        let tiles: &[TileId] = &[][..];
        let mut iter = tiles.group_by_edition();
        assert_eq!(iter.tiles(), &[][..]);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.tiles(), &[][..]);

        let tiles = [tile!(101)];
        let editions: Vec<_> = tiles.group_by_edition().collect();
        assert_eq!(&editions, &[Edition::GtCoreBox][..]);
        let tiles = [tile!(101), tile!(103, a), tile!(103, b)];
        let editions: Vec<_> = tiles.group_by_edition().collect();
        assert_eq!(&editions, &[Edition::GtCoreBox][..]);
        let tiles = [tile!(101), tile!(121, a), tile!(121, b)];
        let editions: Vec<_> = tiles.group_by_edition().collect();
        assert_eq!(&editions, &[Edition::GtChampionship, Edition::GtCoreBox][..]);
        let tiles = [tile!(101), tile!(124, a), tile!(124, b)];
        let editions: Vec<_> = tiles.group_by_edition().collect();
        assert_eq!(&editions, &[Edition::GtCoreBox, Edition::GtChampionship, Edition::GtWorldTour][..]);

        let tiles = [tile!(201, a), tile!(224, a), tile!(304, b), tile!(905, b)];
        let editions: Vec<_> = tiles.group_by_edition().collect();
        assert_eq!(&editions, &[Edition::DirtCoreBox, Edition::Dirt110Percent, Edition::DirtCopilotPack][..]);
        let tiles = [tile!(201, a), tile!(224, a), tile!(224, b), tile!(405, b)];
        let editions: Vec<_> = tiles.group_by_edition().collect();
        assert_eq!(&editions, &[Edition::DirtCoreBox, Edition::DirtRx, Edition::DirtCoreBox][..]);

        let tiles = [tile!(999, a)];
        let mut iter = tiles.group_by_edition();
        assert_eq!(iter.tiles(), &[][..]);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.tiles(), &[tile!(999, a)]);
    }

    #[test]
    fn tile_list_edition_summary() {
        let tiles: &[TileId] = &[][..];
        let summary = tiles.edition_summary();
        assert_eq!(summary, vec![]);
        let series = tiles.detect_series();
        assert_eq!(series, None);

        let tiles = [tile!(101)];
        let summary = tiles.edition_summary();
        assert_eq!(summary, vec![
            EditionSummary::new(Some(Edition::GtCoreBox), 1, 1),
        ]);
        let series = tiles.detect_series();
        assert_eq!(series, Some(Series::Gt));

        let tiles = [tile!(101), tile!(103, a), tile!(103, b)];
        let summary = tiles.edition_summary();
        assert_eq!(summary, vec![
            EditionSummary::new(Some(Edition::GtCoreBox), 1, 3),
        ]);
        let series = tiles.detect_series();
        assert_eq!(series, Some(Series::Gt));

        let tiles = [tile!(101), tile!(121, a), tile!(121, b)];
        let summary = tiles.edition_summary();
        assert_eq!(summary, vec![
            EditionSummary::new(Some(Edition::GtCoreBox), 1, 1),
            EditionSummary::new(Some(Edition::GtChampionship), 1, 2),
        ]);
        let series = tiles.detect_series();
        assert_eq!(series, Some(Series::Gt));

        let tiles = [tile!(101), tile!(124, a), tile!(124, b)];
        let summary = tiles.edition_summary();
        assert_eq!(summary, vec![
            EditionSummary::new(Some(Edition::GtCoreBox), 1, 1),
            EditionSummary::new(Some(Edition::GtChampionship), 1, 1),
            EditionSummary::new(Some(Edition::GtWorldTour), 1, 1),
        ]);
        let series = tiles.detect_series();
        assert_eq!(series, Some(Series::Gt));

        let tiles = [tile!(201, a), tile!(224, a), tile!(304, b), tile!(905, b)];
        let summary = tiles.edition_summary();
        assert_eq!(summary, vec![
            EditionSummary::new(Some(Edition::DirtCoreBox), 1, 2),
            EditionSummary::new(Some(Edition::Dirt110Percent), 1, 1),
            EditionSummary::new(Some(Edition::DirtCopilotPack), 1, 1),
        ]);
        let series = tiles.detect_series();
        assert_eq!(series, Some(Series::Dirt));

        let tiles = [tile!(201, a), tile!(224, a), tile!(224, b), tile!(405, b)];
        let summary = tiles.edition_summary();
        assert_eq!(summary, vec![
            EditionSummary::new(Some(Edition::DirtCoreBox), 2, 3),
            EditionSummary::new(Some(Edition::DirtRx), 1, 1),
        ]);
        let series = tiles.detect_series();
        assert_eq!(series, Some(Series::Dirt));

        let tiles = [tile!(999, a)];
        let summary = tiles.edition_summary();
        assert_eq!(summary, vec![
            EditionSummary::new(None, 1, 1),
        ]);
        let series = tiles.detect_series();
        assert_eq!(series, None);

        let tiles: Vec<TileId> = TileInfo::iter()
            .map(|info| info.full_id())
            .collect();
        let summary = tiles.edition_summary();
        assert_eq!(summary, vec![
            EditionSummary::new(Some(Edition::GtCoreBox), 2, 39),
            EditionSummary::new(Some(Edition::GtChampionship), 2, 17),
            EditionSummary::new(Some(Edition::GtWorldTour), 2, 19),
            EditionSummary::new(Some(Edition::GtTeamChallenge), 2, 24),
            EditionSummary::new(Some(Edition::GtAdrenalinePack), 2, 10),
            EditionSummary::new(Some(Edition::DirtCoreBox), 2, 64),
            EditionSummary::new(Some(Edition::Dirt110Percent), 2, 48),
            EditionSummary::new(Some(Edition::DirtRx), 2, 30),
            EditionSummary::new(Some(Edition::DirtClimb), 2, 8),
            EditionSummary::new(Some(Edition::DirtCopilotPack), 2, 12),
        ]);
        let series = tiles.detect_series();
        assert_eq!(series, None);
    }

    #[test]
    fn tile_list_group_by_terrain() {
        let tiles = [
            tile!(220, a), tile!(231, b), tile!(211, b), tile!(301, a),
            tile!(219, a), tile!(418, a), tile!(311, b), tile!(419, b),
            tile!(208, a), tile!(232, a), tile!(326, b), tile!(302, b),
        ];
        let mut iter = tiles.group_by_terrain();
        assert_eq!(iter.next(), Some(Terrain::Asphalt));
        assert_eq!(iter.tiles(), &[
            tile!(301, a), tile!(418, a),
        ]);
        assert_eq!(iter.next(), Some(Terrain::Gravel));
        assert_eq!(iter.tiles(), &[
            tile!(220, a), tile!(231, b), tile!(211, b), tile!(219, a),
            tile!(419, b), tile!(208, a), tile!(232, a),
        ]);
        assert_eq!(iter.next(), Some(Terrain::Snow));
        assert_eq!(iter.tiles(), &[
            tile!(311, b), tile!(326, b), tile!(302, b),
        ]);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.tiles(), &[][..]);

        let tiles = [tile!(101), tile!(999, a)];
        let mut iter = tiles.group_by_terrain();
        assert_eq!(iter.next(), Some(Terrain::None));
        assert_eq!(iter.tiles(), &[tile!(101)]);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.tiles(), &[tile!(999, a)]);
    }

    #[test]
    fn tile_list_terrain_summary() {
        let tiles = [
            tile!(220, a), tile!(231, b), tile!(211, b), tile!(301, a),
            tile!(219, a), tile!(418, a), tile!(311, b), tile!(419, b),
            tile!(208, a), tile!(232, a), tile!(326, b), tile!(302, b),
        ];
        let summary = tiles.terrain_summary();
        assert_eq!(summary, [
            TerrainSummary::new(Some(Terrain::Asphalt), 2),
            TerrainSummary::new(Some(Terrain::Gravel), 7),
            TerrainSummary::new(Some(Terrain::Snow), 3),
        ]);

        let tiles = [tile!(101), tile!(999, a)];
        let summary = tiles.terrain_summary();
        assert_eq!(summary, [
            TerrainSummary::new(Some(Terrain::None), 1),
            TerrainSummary::new(None, 1),
        ]);

        let tiles: Vec<TileId> = TileInfo::iter()
            .map(|info| info.full_id())
            .collect();
        let summary = tiles.terrain_summary();
        assert_eq!(summary, [
            TerrainSummary::new(Some(Terrain::None), 13),
            TerrainSummary::new(Some(Terrain::Asphalt), 152),
            TerrainSummary::new(Some(Terrain::Gravel), 80),
            TerrainSummary::new(Some(Terrain::Snow), 26),
        ]);
    }

    #[test]
    fn tile_list_group_by_danger_level() {
        let tiles = [
            tile!(220, a), tile!(231, b), tile!(211, b), tile!(301, a),
            tile!(219, a), tile!(418, a), tile!(311, b), tile!(419, b),
            tile!(208, a), tile!(232, a), tile!(326, b), tile!(302, b),
        ];
        let mut iter = tiles.group_by_danger_level();
        assert_eq!(iter.next(), Some(DangerLevel::Low));
        assert_eq!(iter.tiles(), &[
            tile!(301, a), tile!(208, a),
        ]);
        assert_eq!(iter.next(), Some(DangerLevel::Medium));
        assert_eq!(iter.tiles(), &[
            tile!(231, b), tile!(211, b), tile!(219, a), tile!(311, b),
            tile!(302, b),
        ]);
        assert_eq!(iter.next(), Some(DangerLevel::High));
        assert_eq!(iter.tiles(), &[
            tile!(220, a), tile!(418, a), tile!(419, b), tile!(232, a),
            tile!(326, b),
        ]);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.tiles(), &[][..]);

        let tiles = [tile!(101), tile!(999, a)];
        let mut iter = tiles.group_by_danger_level();
        assert_eq!(iter.next(), Some(DangerLevel::None));
        assert_eq!(iter.tiles(), &[tile!(101)]);
        assert_eq!(iter.next(), None);
        assert_eq!(iter.tiles(), &[tile!(999, a)]);
    }

    #[test]
    fn tile_list_danger_level_summary() {
        let tiles = [
            tile!(220, a), tile!(231, b), tile!(211, b), tile!(301, a),
            tile!(219, a), tile!(418, a), tile!(311, b), tile!(419, b),
            tile!(208, a), tile!(232, a), tile!(326, b), tile!(302, b),
        ];
        let summary = tiles.danger_level_summary();
        assert_eq!(summary, [
            DangerLevelSummary::new(Some(DangerLevel::Low), 2),
            DangerLevelSummary::new(Some(DangerLevel::Medium), 5),
            DangerLevelSummary::new(Some(DangerLevel::High), 5),
        ]);

        let tiles = [tile!(101), tile!(999, a)];
        let summary = tiles.danger_level_summary();
        assert_eq!(summary, [
            DangerLevelSummary::new(Some(DangerLevel::None), 1),
            DangerLevelSummary::new(None, 1),
        ]);

        let tiles: Vec<TileId> = TileInfo::iter()
            .map(|info| info.full_id())
            .collect();
        let summary = tiles.danger_level_summary();
        assert_eq!(summary, [
            DangerLevelSummary::new(Some(DangerLevel::None), 13),
            DangerLevelSummary::new(Some(DangerLevel::Low), 81),
            DangerLevelSummary::new(Some(DangerLevel::Medium), 115),
            DangerLevelSummary::new(Some(DangerLevel::High), 62),
        ]);
    }

    #[test]
    fn connection_hint_from_str() {
        assert_eq!("".parse::<ConnectionHint>(), Ok(ConnectionHint::Straight));
        assert_eq!("s".parse::<ConnectionHint>(), Ok(ConnectionHint::Straight));
        assert_eq!("S".parse::<ConnectionHint>(), Ok(ConnectionHint::Straight));
        assert_eq!("l".parse::<ConnectionHint>(), Ok(ConnectionHint::Left));
        assert_eq!("L".parse::<ConnectionHint>(), Ok(ConnectionHint::Left));
        assert_eq!("r".parse::<ConnectionHint>(), Ok(ConnectionHint::Right));
        assert_eq!("R".parse::<ConnectionHint>(), Ok(ConnectionHint::Right));

        assert!("x".parse::<ConnectionHint>().is_err());
    }

    #[test]
    fn tile_chicane_hint() {
        let chicane_tiles = [
            tile!(110, a),
            tile!(110, b),
            tile!(116, a),
            tile!(116, b),
            tile!(214, a),
            tile!(214, b),
            tile!(226, a),
            tile!(226, b),
            tile!(411, a),
            tile!(411, b),
        ];
        for &tile in &chicane_tiles {
            let info = TileInfo::get(tile).unwrap();
            assert!(info.connection(Direction::A) == ConnectionHint::Left);
            assert!(info.connection(Direction::D) == ConnectionHint::Right);
        }
    }

    #[test]
    fn connection_orientation() {
        let conn = Connection::None;
        assert_eq!(conn == ConnectionHint::Straight, false);
        assert_eq!(conn == ConnectionHint::Left, false);
        assert_eq!(conn == ConnectionHint::Right, false);

        let conn = Connection::Straight(0);
        assert_eq!(conn == ConnectionHint::Straight, true);
        assert_eq!(conn == ConnectionHint::Left, false);
        assert_eq!(conn == ConnectionHint::Right, false);

        let conn = Connection::Straight(1);
        assert_eq!(conn == ConnectionHint::Straight, true);
        assert_eq!(conn == ConnectionHint::Left, false);
        assert_eq!(conn == ConnectionHint::Right, false);

        let conn = Connection::Straight(-1);
        assert_eq!(conn == ConnectionHint::Straight, true);
        assert_eq!(conn == ConnectionHint::Left, false);
        assert_eq!(conn == ConnectionHint::Right, false);

        let conn = Connection::Left(0);
        assert_eq!(conn == ConnectionHint::Straight, false);
        assert_eq!(conn == ConnectionHint::Left, true);
        assert_eq!(conn == ConnectionHint::Right, false);

        let conn = Connection::Left(1);
        assert_eq!(conn == ConnectionHint::Straight, false);
        assert_eq!(conn == ConnectionHint::Left, true);
        assert_eq!(conn == ConnectionHint::Right, false);

        let conn = Connection::Left(2);
        assert_eq!(conn == ConnectionHint::Straight, false);
        assert_eq!(conn == ConnectionHint::Left, true);
        assert_eq!(conn == ConnectionHint::Right, false);

        let conn = Connection::Right(0);
        assert_eq!(conn == ConnectionHint::Straight, false);
        assert_eq!(conn == ConnectionHint::Left, false);
        assert_eq!(conn == ConnectionHint::Right, true);

        let conn = Connection::Right(1);
        assert_eq!(conn == ConnectionHint::Straight, false);
        assert_eq!(conn == ConnectionHint::Left, false);
        assert_eq!(conn == ConnectionHint::Right, true);

        let conn = Connection::Right(2);
        assert_eq!(conn == ConnectionHint::Straight, false);
        assert_eq!(conn == ConnectionHint::Left, false);
        assert_eq!(conn == ConnectionHint::Right, true);

        let conn = Connection::Junction(0, -1);
        assert_eq!(conn == ConnectionHint::Straight, false);
        assert_eq!(conn == ConnectionHint::Left, true);
        assert_eq!(conn == ConnectionHint::Right, false);

        let conn = Connection::Junction(-1, 2);
        assert_eq!(conn == ConnectionHint::Straight, false);
        assert_eq!(conn == ConnectionHint::Left, true);
        assert_eq!(conn == ConnectionHint::Right, false);

        let conn = Connection::Junction(0, 2);
        assert_eq!(conn == ConnectionHint::Straight, false);
        assert_eq!(conn == ConnectionHint::Left, false);
        assert_eq!(conn == ConnectionHint::Right, true);

        let conn = Connection::Junction(1, -2);
        assert_eq!(conn == ConnectionHint::Straight, false);
        assert_eq!(conn == ConnectionHint::Left, false);
        assert_eq!(conn == ConnectionHint::Right, true);
    }

    #[test]
    fn connection_target() {
        let conn = Connection::None;
        assert_eq!(conn.target(Direction::A), None);
        assert_eq!(conn.target(Direction::D), None);
        assert_eq!(conn.target(Direction::F), None);

        let conn = Connection::Straight(0);
        assert_eq!(conn.target(Direction::A), Some(Direction::D));
        assert_eq!(conn.target(Direction::D), Some(Direction::A));
        assert_eq!(conn.target(Direction::F), Some(Direction::C));

        let conn = Connection::Straight(1);
        assert_eq!(conn.target(Direction::A), Some(Direction::E));
        assert_eq!(conn.target(Direction::D), Some(Direction::B));
        assert_eq!(conn.target(Direction::F), Some(Direction::D));

        let conn = Connection::Straight(2);
        assert_eq!(conn.target(Direction::A), Some(Direction::F));
        assert_eq!(conn.target(Direction::D), Some(Direction::C));
        assert_eq!(conn.target(Direction::F), Some(Direction::E));

        let conn = Connection::Straight(3);
        assert_eq!(conn.target(Direction::A), Some(Direction::A));
        assert_eq!(conn.target(Direction::D), Some(Direction::D));
        assert_eq!(conn.target(Direction::F), Some(Direction::F));

        let conn = Connection::Straight(-1);
        assert_eq!(conn.target(Direction::A), Some(Direction::C));
        assert_eq!(conn.target(Direction::D), Some(Direction::F));
        assert_eq!(conn.target(Direction::F), Some(Direction::B));

        let conn = Connection::Left(0);
        assert_eq!(conn.target(Direction::A), Some(Direction::D));
        assert_eq!(conn.target(Direction::D), Some(Direction::A));
        assert_eq!(conn.target(Direction::F), Some(Direction::C));

        let conn = Connection::Left(1);
        assert_eq!(conn.target(Direction::A), Some(Direction::C));
        assert_eq!(conn.target(Direction::D), Some(Direction::F));
        assert_eq!(conn.target(Direction::F), Some(Direction::B));

        let conn = Connection::Left(2);
        assert_eq!(conn.target(Direction::A), Some(Direction::B));
        assert_eq!(conn.target(Direction::D), Some(Direction::E));
        assert_eq!(conn.target(Direction::F), Some(Direction::A));

        let conn = Connection::Right(0);
        assert_eq!(conn.target(Direction::A), Some(Direction::D));
        assert_eq!(conn.target(Direction::D), Some(Direction::A));
        assert_eq!(conn.target(Direction::F), Some(Direction::C));

        let conn = Connection::Right(1);
        assert_eq!(conn.target(Direction::A), Some(Direction::E));
        assert_eq!(conn.target(Direction::D), Some(Direction::B));
        assert_eq!(conn.target(Direction::F), Some(Direction::D));

        let conn = Connection::Right(2);
        assert_eq!(conn.target(Direction::A), Some(Direction::F));
        assert_eq!(conn.target(Direction::D), Some(Direction::C));
        assert_eq!(conn.target(Direction::F), Some(Direction::E));

        let conn = Connection::Junction(0, -1);
        assert_eq!(conn.target(Direction::A), Some(Direction::D));
        assert_eq!(conn.target(Direction::D), Some(Direction::A));
        assert_eq!(conn.target(Direction::F), Some(Direction::C));

        let conn = Connection::Junction(-1, 2);
        assert_eq!(conn.target(Direction::A), Some(Direction::C));
        assert_eq!(conn.target(Direction::D), Some(Direction::F));
        assert_eq!(conn.target(Direction::F), Some(Direction::B));

        let conn = Connection::Junction(0, 2);
        assert_eq!(conn.target(Direction::A), Some(Direction::D));
        assert_eq!(conn.target(Direction::D), Some(Direction::A));
        assert_eq!(conn.target(Direction::F), Some(Direction::C));

        let conn = Connection::Junction(1, -2);
        assert_eq!(conn.target(Direction::A), Some(Direction::E));
        assert_eq!(conn.target(Direction::D), Some(Direction::B));
        assert_eq!(conn.target(Direction::F), Some(Direction::D));
    }

    #[test]
    fn terrain_to_str() {
        assert_eq!(Terrain::None.to_string(), "None");
        assert_eq!(Terrain::Asphalt.to_string(), "Asphalt");
        assert_eq!(Terrain::Gravel.to_string(), "Gravel");
        assert_eq!(Terrain::Snow.to_string(), "Snow");

        let text = format!("{:x}", Terrain::Gravel);
        assert_eq!(text, "gravel");
        let text = format!("{:x}", Terrain::Snow);
        assert_eq!(text, "snow");
    }

    #[test]
    fn terrain_from_str() {
        assert_eq!("None".parse::<Terrain>(), Ok(Terrain::None));
        assert_eq!("asphalt".parse::<Terrain>(), Ok(Terrain::Asphalt));
        assert_eq!("Gravel".parse::<Terrain>(), Ok(Terrain::Gravel));
        assert_eq!("SNOW".parse::<Terrain>(), Ok(Terrain::Snow));

        assert!("".parse::<Terrain>().is_err());
        assert!("x".parse::<Terrain>().is_err());
        assert!("None-1".parse::<Terrain>().is_err());
        assert!("Asphalt x".parse::<Terrain>().is_err());
    }

    #[test]
    fn terrain_serde() {
        let terrain = Terrain::None;
        let text = serde_json::to_string(&terrain).unwrap();
        assert_eq!(text, r#""none""#);

        let terrain = Terrain::Asphalt;
        let text = serde_json::to_string(&terrain).unwrap();
        assert_eq!(text, r#""asphalt""#);

        let terrain = Terrain::Gravel;
        let text = serde_json::to_string(&terrain).unwrap();
        assert_eq!(text, r#""gravel""#);

        let text = r#""snow""#;
        let terrain: Terrain = serde_json::from_str(text).unwrap();
        assert_eq!(terrain, Terrain::Snow);

        let text = r#""#;
        let result: Result<Terrain, _> = serde_json::from_str(text);
        assert!(result.is_err());

        let text = r#""none-1""#;
        let result: Result<Terrain, _> = serde_json::from_str(text);
        assert!(result.is_err());
    }

    #[test]
    fn danger_level_to_str() {
        assert_eq!(DangerLevel::None.to_string(), "None");
        assert_eq!(DangerLevel::Low.to_string(), "Low");
        assert_eq!(DangerLevel::Medium.to_string(), "Medium");
        assert_eq!(DangerLevel::High.to_string(), "High");

        let text = format!("{:x}", DangerLevel::Medium);
        assert_eq!(text, "medium");
        let text = format!("{:x}", DangerLevel::High);
        assert_eq!(text, "high");
    }

    #[test]
    fn danger_level_from_str() {
        assert_eq!("None".parse::<DangerLevel>(), Ok(DangerLevel::None));
        assert_eq!("low".parse::<DangerLevel>(), Ok(DangerLevel::Low));
        assert_eq!("Medium".parse::<DangerLevel>(), Ok(DangerLevel::Medium));
        assert_eq!("HIGH".parse::<DangerLevel>(), Ok(DangerLevel::High));

        assert!("".parse::<DangerLevel>().is_err());
        assert!("x".parse::<DangerLevel>().is_err());
        assert!("None-1".parse::<DangerLevel>().is_err());
        assert!("Low x".parse::<DangerLevel>().is_err());
    }

    #[test]
    fn danger_level_serde() {
        let danger_level = DangerLevel::None;
        let text = serde_json::to_string(&danger_level).unwrap();
        assert_eq!(text, r#""none""#);

        let danger_level = DangerLevel::Low;
        let text = serde_json::to_string(&danger_level).unwrap();
        assert_eq!(text, r#""low""#);

        let danger_level = DangerLevel::Medium;
        let text = serde_json::to_string(&danger_level).unwrap();
        assert_eq!(text, r#""medium""#);

        let text = r#""high""#;
        let danger_level: DangerLevel = serde_json::from_str(text).unwrap();
        assert_eq!(danger_level, DangerLevel::High);

        let text = r#""#;
        let result: Result<DangerLevel, _> = serde_json::from_str(text);
        assert!(result.is_err());

        let text = r#""none-1""#;
        let result: Result<DangerLevel, _> = serde_json::from_str(text);
        assert!(result.is_err());
    }

    #[test]
    fn pacenote_to_str() {
        assert_eq!(Pacenote::Limit(3).to_string(), "3");
        assert_eq!(Pacenote::LimitHazard(4).to_string(), "4!");
        assert_eq!(Pacenote::Hazard.to_string(), "Hazard");
        assert_eq!(Pacenote::Shortcut(2).to_string(), "Cut 2");
        assert_eq!(Pacenote::Jump(5).to_string(), "Jump 5");
        assert_eq!(Pacenote::Water.to_string(), "Water");
        assert_eq!(Pacenote::Climb.to_string(), "Climb");
        assert_eq!(Pacenote::Chicane.to_string(), "Chicane");
        assert_eq!(Pacenote::Cloud.to_string(), "Cloud");
        assert_eq!(Pacenote::Oxygen(1).to_string(), "Oxygen -1");
        assert_eq!(Pacenote::MudSpray.to_string(), "Mud");
    }

    #[test]
    fn tile_info_get() {
        assert!(TileInfo::get("0".parse().unwrap()).is_none());
        assert!(TileInfo::get("101".parse().unwrap()).is_some());
        assert!(TileInfo::get("102a".parse().unwrap()).is_some());
        assert!(TileInfo::get("102b".parse().unwrap()).is_some());
        assert!(TileInfo::get("103a-1".parse().unwrap()).is_some());
        assert!(TileInfo::get("103b-3".parse().unwrap()).is_some());
    }

    #[test]
    fn tile_info_count() {
        // check total count of tiles
        assert_eq!(TILE_INFOS.iter().map(|info| info.count as u32).sum::<u32>(), 307);

        // check same count of variants for "a" and "b" side
        for info in TILE_INFOS.iter() {
            let mut pair_id = info.base_id();
            pair_id.side = match pair_id.side() {
                0 => 0,
                1 => 2,
                2 => 1,
                _ => unimplemented!(),
            };
            let pair = TileInfo::get(pair_id).unwrap();
            assert_eq!(info.count, pair.count, "tile info {} variant count does not match the other side", info.id);
        }
    }

    #[test]
    fn tile_info_sorted() {
        for info in TILE_INFOS.iter() {
            assert!(info.id == info.id.base(), "tile info is defined with non-base identifier {}", info.id);
        }
        for infos in TILE_INFOS.windows(2) {
            assert!(infos[0].id <= infos[1].id, "tile info is not sorted strictly ({} > {})", infos[0].id, infos[1].id);
        }
    }

    #[test]
    fn tile_info_terrain() {
        for info in TILE_INFOS.iter() {
            let danger_level = info.danger_level();
            if info.terrain() == Terrain::None {
                assert_eq!(danger_level, DangerLevel::None, "tile info {} has undefined terrain with non-zero danger level", info.id);
            }
        }
    }

    #[test]
    fn tile_info_pacenotes() {
        for info in TILE_INFOS.iter() {
            let pacenotes = info.pacenotes();
            for note in pacenotes {
                match note {
                    Pacenote::Limit(val) | Pacenote::LimitHazard(val) => {
                        assert!(val > 0 && val <= 6, "tile info {} has pacenote with invalid speed limit", info.id);
                    },
                    Pacenote::Hazard => (),
                    Pacenote::Shortcut(val) => {
                        assert!(val > 0 && val <= 6, "tile info {} has pacenote with invalid shortcut speed limit", info.id);
                    },
                    Pacenote::Jump(val) => {
                        assert!(val > 0 && val <= 6, "tile info {} has pacenote with invalid jump value", info.id);
                    },
                    Pacenote::Water | Pacenote::Climb | Pacenote::Chicane | Pacenote::Cloud => (),
                    Pacenote::Oxygen(val) => {
                        assert!(val > 0 && val <= 3, "tile info {} has pacenote with invalid oxygen value", info.id);
                    },
                    Pacenote::MudSpray => (),
                }
            }
        }
    }

    #[test]
    fn tile_info_connection_edge() {
        for tile in TILE_INFOS.iter() {
            for dir in Direction::iter() {
                let has_connection = !matches!(tile.connection(*dir), Connection::None);
                let has_edge = !matches!(tile.edge(*dir), Edge::None);
                assert_eq!(has_connection, has_edge,
                    "tile {}, direction {}: connection does not match according edge", tile, dir);

                let lanes = tile.edge(*dir).lanes();
                if has_connection {
                    assert!(lanes > 0,
                        "tile {}, direction {}: side with connection must have a non-zero number of lanes", tile, dir);
                } else {
                    assert_eq!(lanes, 0,
                        "tile {}, direction {}: side without connection cannot have lanes", tile, dir);
                }
            }
        }
    }

    #[test]
    fn tile_info_series() {
        for info in TILE_INFOS.iter() {
            // will panic if not defined
            let _series = info.series();
        }
    }

    #[test]
    fn tile_info_editions() {
        for info in TILE_INFOS.iter() {
            let editions = info.editions();
            assert!(!editions.is_empty(), "tile info {} has no editions", info.id);
            assert!(editions.len() <= 2, "tile info {} has more than two editions", info.id);
        }
    }
}

//----------------------------------------------------------------------------
