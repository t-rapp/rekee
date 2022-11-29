//----------------------------------------------------------------------------
//! Map of tiles within a hexagon grid.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use std::fmt;

use serde::{Serialize, Deserialize, Deserializer};
use serde::de::{self, Visitor};

use crate::edition::Edition;
use crate::hexagon::{Coordinate, Direction, FloatCoordinate, FloatDirection, Layout, Point};
use crate::tile::{Connection, ConnectionHint, Edge, Terrain, TileId, TileInfo};
use crate::token::TokenId;

//----------------------------------------------------------------------------

/// Single tile that is part of the map. A placed tile consists of tile
/// identifier, grid coordinates, rotation direction, and optionally a list
/// of tokens.
///
/// Contains some private map helper functions that apply tile information
/// to the placed tile.
#[derive(Clone, Serialize)]
pub struct PlacedTile {
    id: TileId,
    #[serde(flatten)]
    pub pos: Coordinate,
    pub dir: Direction,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub tokens: Vec<PlacedToken>,
    #[serde(skip_serializing)]
    info: Option<&'static TileInfo>,
}

impl PlacedTile {
    /// Creates a new tile with identifier, coordinates, and rotation.
    pub fn new(id: TileId, pos: Coordinate, dir: Direction) -> Self {
        let tokens = Vec::new();
        PlacedTile::with_tokens(id, pos, dir, tokens)
    }

    /// Creates a new tile with identifier, coordinates, rotation, and a list
    /// of tokens.
    pub fn with_tokens(id: TileId, pos: Coordinate, dir: Direction, tokens: Vec<PlacedToken>) -> Self {
        let info = TileInfo::get(id);
        PlacedTile { id, pos, dir, tokens, info }
    }

    /// Tile identifier.
    pub fn id(&self) -> TileId {
        self.id
    }

    /// Terrain information for a tile.
    ///
    /// Returns `Some(Terrain)` for tiles that have internal terrain surface
    /// information, otherwise returns `None`.
    ///
    /// # Examples
    ///
    /// ```
    /// # #[macro_use] extern crate rekee;
    /// # use rekee::hexagon::{Coordinate, Direction};
    /// # use rekee::map::PlacedTile;
    /// # use rekee::tile::{Terrain, TileId};
    ///
    /// let tile = PlacedTile::new(tile!(301, a), Coordinate::new(0, 0), Direction::A);
    /// assert_eq!(tile.terrain(), Some(Terrain::Asphalt));
    ///
    /// let tile = PlacedTile::new(tile!(302, b), Coordinate::new(0, 0), Direction::A);
    /// assert_eq!(tile.terrain(), Some(Terrain::Snow));
    ///
    /// let tile = PlacedTile::new(tile!(999, a), Coordinate::new(0, 0), Direction::A);
    /// assert_eq!(tile.terrain(), None);
    /// ```
    pub fn terrain(&self) -> Option<Terrain> {
        self.info.map(|info| info.terrain())
    }

    /// Check whether the tile has tokens placed on it.
    ///
    /// Considers only "real" tokens that lay flat on the tile and change the
    /// track spaces, ignores standee tokens from DIRT RX expansion.
    ///
    /// Used for rendering a "modified" indicator suffix on map tile labels.
    pub fn has_flat_tokens(&self) -> bool {
        self.tokens.iter()
            .any(|token| token.id.edition() != Edition::DirtRx)
    }

    fn connection(&self, dir: Direction) -> Connection {
        if let Some(info) = self.info {
            info.connection(dir - self.dir)
        } else {
            Connection::None
        }
    }

    fn connection_target(&self, source: Direction) -> Option<Direction> {
        if let Some(info) = self.info {
            info.connection_target(source - self.dir)
                .map(|dir| dir + self.dir)
        } else {
            None
        }
    }

    fn edge(&self, dir: Direction) -> Edge {
        if let Some(info) = self.info {
            info.edge(dir - self.dir)
        } else {
            Edge::None
        }
    }
}

impl fmt::Debug for PlacedTile {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("PlacedTile")
            .field("id", &self.id)
            .field("pos", &self.pos)
            .field("dir", &self.dir)
            .field("tokens", &self.tokens)
            // skip printing cached tile info
            .finish()
    }
}

impl PartialEq<PlacedTile> for PlacedTile {
    fn eq(&self, other: &PlacedTile) -> bool {
        // skip comparing cached tile info reference
        self.id == other.id && self.pos == other.pos && self.dir == other.dir && self.tokens == other.tokens
    }
}

#[cfg(test)]
impl approx::AbsDiffEq for PlacedTile {
    type Epsilon = f32;

    fn default_epsilon() -> f32 {
        tests::EPSILON
    }

    fn abs_diff_eq(&self, other: &Self, epsilon: f32) -> bool {
        self.id.eq(&other.id) &&
        self.pos.eq(&other.pos) &&
        self.dir.eq(&other.dir) &&
        self.tokens[..].abs_diff_eq(&other.tokens[..], epsilon)
    }
}

impl AsRef<TileId> for PlacedTile {
    fn as_ref(&self) -> &TileId {
        &self.id
    }
}

impl<'de> Deserialize<'de> for PlacedTile {
    fn deserialize<D>(deserializer: D) -> Result<PlacedTile, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "lowercase")]
        enum Field { Id, Q, R, Dir, Tokens }

        struct PlacedTileVisitor;

        impl<'de> Visitor<'de> for PlacedTileVisitor {
            type Value = PlacedTile;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a placed tile structure")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<PlacedTile, V::Error>
            where
                V: de::SeqAccess<'de>,
            {
                let id = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                let q = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(1, &self))?;
                let r = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(2, &self))?;
                let dir = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(3, &self))?;
                let tokens: Vec<PlacedToken> = seq.next_element()?
                    .unwrap_or_default();
                Ok(PlacedTile::with_tokens(id, (q, r).into(), dir, tokens))
            }

            fn visit_map<V>(self, mut map: V) -> Result<PlacedTile, V::Error>
            where
                V: de::MapAccess<'de>,
            {
                let mut id = None;
                let mut q = None;
                let mut r = None;
                let mut dir = None;
                let mut tokens = None;
                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Id => {
                            if id.is_some() {
                                return Err(de::Error::duplicate_field("id"));
                            }
                            id = Some(map.next_value()?);
                        }
                        Field::Q => {
                            if q.is_some() {
                                return Err(de::Error::duplicate_field("q"));
                            }
                            q = Some(map.next_value()?);
                        }
                        Field::R => {
                            if r.is_some() {
                                return Err(de::Error::duplicate_field("r"));
                            }
                            r = Some(map.next_value()?);
                        }
                        Field::Dir => {
                            if dir.is_some() {
                                return Err(de::Error::duplicate_field("dir"));
                            }
                            dir = Some(map.next_value()?);
                        }
                        Field::Tokens => {
                            if tokens.is_some() {
                                return Err(de::Error::duplicate_field("tokens"));
                            }
                            tokens = Some(map.next_value()?);
                        }
                    }
                }
                let id = id.ok_or_else(|| de::Error::missing_field("id"))?;
                let q = q.ok_or_else(|| de::Error::missing_field("q"))?;
                let r = r.ok_or_else(|| de::Error::missing_field("r"))?;
                let dir = dir.ok_or_else(|| de::Error::missing_field("dir"))?;
                let tokens = tokens.unwrap_or_default();
                Ok(PlacedTile::with_tokens(id, (q, r).into(), dir, tokens))
            }
        }

        const FIELDS: [&str; 5] = ["id", "q", "r", "dir", "tokens"];
        deserializer.deserialize_struct("tile", &FIELDS, PlacedTileVisitor)
    }
}

//----------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PlacedToken {
    pub id: TokenId,
    #[serde(flatten)]
    pub pos: FloatCoordinate,
    pub dir: FloatDirection,
}

impl PlacedToken {
    /// Creates a new token with identifier, coordinates, and rotation.
    pub fn new(id: TokenId, pos: FloatCoordinate, dir: FloatDirection) -> Self {
        PlacedToken { id, pos, dir }
    }
}

#[cfg(test)]
impl approx::AbsDiffEq for PlacedToken {
    type Epsilon = f32;

    fn default_epsilon() -> f32 {
        tests::EPSILON
    }

    fn abs_diff_eq(&self, other: &Self, epsilon: f32) -> bool {
        self.id.eq(&other.id) &&
        self.pos.abs_diff_eq(&other.pos, epsilon) &&
        self.dir.abs_diff_eq(&other.dir, epsilon)
    }
}

//----------------------------------------------------------------------------

/// Map for storing track tiles.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Map {
    title: String,
    tiles: Vec<PlacedTile>,
    #[serde(default)]
    active_pos: Coordinate,
    #[serde(default = "Map::default_active_dir")]
    active_dir: Direction,
}

impl Map {
    /// Creates a new and empty map.
    pub fn new() -> Self {
        let title = "My Track".to_string();
        let tiles = Vec::new();
        let active_pos = Coordinate::default();
        let active_dir = Self::default_active_dir();
        Map { title, tiles, active_pos, active_dir }
    }

    /// Map title.
    pub fn title(&self) -> &str {
        &self.title
    }

    /// Updates the map title.
    pub fn set_title(&mut self, title: &str) {
        self.title = title.to_string();
    }

    /// List of all tiles placed on the map.
    pub fn tiles(&self) -> &[PlacedTile] {
        &self.tiles
    }

    /// Active position for the next tile `append` action.
    /// Returns `None` if there currently is no active position.
    pub fn active_pos(&self) -> Option<Coordinate> {
        if self.get(self.active_pos).is_none() {
            Some(self.active_pos)
        } else {
            None
        }
    }

    /// Update position for the next tile `append` action.
    pub fn set_active_pos(&mut self, pos: Coordinate) {
        let mut next_active_pos = pos;
        let mut next_active_dir = Self::default_active_dir();

        let tile = self.get(pos);
        if let Some(tile) = tile {
            // some tile exists at the specified map position
            let mut found = false;
            if self.active_pos == pos {
                // try to follow tile connection using the previous direction
                if let Some(dir) = tile.connection_target(self.active_dir) {
                    next_active_pos = tile.pos.neighbor(dir);
                    next_active_dir = dir.opposite();
                    found = true;
                }
            }
            if !found {
                // lookup and follow first open tile connection
                for dir in Direction::iter().map(|&dir| dir + tile.dir) {
                    if tile.connection_target(dir).is_some() {
                        let neighbor_pos = tile.pos.neighbor(dir);
                        if self.get(neighbor_pos).is_none() {
                            next_active_pos = neighbor_pos;
                            next_active_dir = dir.opposite();
                            break;
                        }
                    }
                }
            }
        } else {
            // map position is empty, lookup first open neighbor direction
            for &dir in Direction::iter() {
                let neighbor_pos = pos.neighbor(dir);
                if let Some(tile) = self.get(neighbor_pos) {
                    let edge = tile.edge(dir.opposite());
                    if edge.lanes() > 0 {
                        next_active_dir = dir;
                        break;
                    }
                }
            }
        }

        debug!("next active pos: {}, dir: {}", next_active_pos, next_active_dir);
        self.active_pos = next_active_pos;
        self.active_dir = next_active_dir;
    }

    /// Active direction for the next tile `append` action.
    pub fn active_dir(&self) -> Direction {
        self.active_dir
    }

    /// Internal helper function for (de)serialization.
    fn default_active_dir() -> Direction {
        Direction::D
    }

    /// Returns tile at the given map position, if existing.
    pub fn get(&self, pos: Coordinate) -> Option<&PlacedTile> {
        self.tiles.iter()
            .find(|tile| tile.pos == pos)
    }

    /// Internal helper function for updating tile tokens.
    fn get_mut(&mut self, pos: Coordinate) -> Option<&mut PlacedTile> {
        self.tiles.iter_mut()
            .find(|tile| tile.pos == pos)
    }

    /// Insert a new tile using the specified position and direction.
    pub fn insert(&mut self, id: TileId, pos: Coordinate, dir: Direction) {
        let tokens = Vec::new();
        self.insert_with_tokens(id, pos, dir, tokens)
    }

    /// Insert a new tile using the specified position and direction, plus some
    /// track tokens that are part of the tile.
    pub fn insert_with_tokens(&mut self, id: TileId, pos: Coordinate, dir: Direction, tokens: Vec<PlacedToken>) {
        // remove any existing tile at insert position
        self.tiles.retain(|tile| tile.pos != pos);

        // auto-select tile id variant, if not given
        let mut id = id;
        if id.var() == 0 {
            let count = self.tiles.iter()
                .filter(|tile| tile.id.base() == id).count();
            match TileInfo::get(id) {
                Some(info) if info.count() > 1 => {
                    let var = ((count % info.count()) + 1) as u8;
                    id = TileId::new(id.num(), id.side(), var);
                },
                _ => (),
            }
        }

        debug!("insert of tile {} at pos: {}, dir: {}, token count: {}", id, pos, dir, tokens.len());
        let tile = PlacedTile::with_tokens(id, pos, dir, tokens);
        self.tiles.push(tile);

        // update active tile append position
        self.set_active_pos(pos);
    }

    /// Append a new tile to the map using the active position. In contrast
    /// to `insert()` this function allows auto-detection of tile position and
    /// direction.
    #[allow(clippy::unnecessary_unwrap)] // false positive, see issue rust-lang/rust-clippy#4530
    pub fn append(&mut self, id: TileId, pos: Option<Coordinate>, hint: Option<ConnectionHint>) -> bool {
        debug!("append tile {}, pos: {:?}, hint: {:?}", id, pos, hint);

        let tile_pos = match pos.or_else(|| self.active_pos()) {
            Some(val) => val,
            None => {
                info!("skipped append of tile '{}' as there is no active insert position", id);
                return false;
            }
        };
        let mut tile_dir = Direction::A;

        let mut neighbor_conns = 0;
        let mut neighbor_dir = Direction::A;
        let mut neighbor_edges = [None; 6];
        for &dir in Direction::iter() {
            let pos = tile_pos.neighbor(dir);
            if let Some(tile) = self.get(pos) {
                let edge = tile.edge(dir.opposite());
                if edge.lanes() > 0 {
                    neighbor_conns += 1;
                    neighbor_dir = dir;
                }
                neighbor_edges[dir] = Some(edge);
            }
        }
        debug!("neighbor connections: {}, dir: {}, edges: {:?}",
            neighbor_conns, neighbor_dir, neighbor_edges);

        if hint.is_some() && tile_pos != self.active_pos && neighbor_conns == 1 {
            // apply hint relative to the single neighbor connection
            self.active_pos = tile_pos;
            self.active_dir = neighbor_dir;
        }

        if let Some(info) = TileInfo::get(id) {
            // find best tile direction based on edges to neighbor tiles
            let mut max_score = 0;
            for &dir in Direction::iter() {
                let mut score = 0;
                for &neighbor_dir in Direction::iter() {
                    let tile_edge = info.edge(neighbor_dir - dir);
                    if let Some(neighbor_edge) = neighbor_edges[neighbor_dir] {
                        // tile has some neighbor in that direction
                        match (tile_edge, neighbor_edge) {
                            (Edge::None, Edge::None) => score += 1,
                            (Edge::SkewLeft(a), Edge::SkewLeft(b)) if a == b => score += 2,
                            (Edge::SkewLeft(_), Edge::SkewLeft(_)) => score += 1,
                            (Edge::SkewRight(a), Edge::SkewRight(b)) if a == b => score += 2,
                            (Edge::SkewRight(_), Edge::SkewRight(_)) => score += 1,
                            (Edge::Straight(a), Edge::Straight(b)) if a == b => score += 2,
                            (Edge::Straight(_), Edge::Straight(_)) => score += 1,
                            _ => (),
                        }
                    } else {
                        // tile has no neighbor in that direction
                        score += 1;
                    }
                }
                if score > max_score {
                    max_score = score;
                    tile_dir = dir;
                }
            }
            debug!("found tile_dir '{}' based on neighbor edges (score: {})", tile_dir, max_score);

            if hint.is_some() && tile_pos == self.active_pos {
                let hint = hint.unwrap();
                // find first inner tile connection that matches the hint
                for &dir in Direction::iter() {
                    let conn = info.connection(self.active_dir - dir);
                    if conn == hint {
                        tile_dir = dir;
                        break;
                    }
                }
                debug!("found tile dir '{}' based on hint '{}', (active dir: {})", tile_dir, hint, self.active_dir);
            }
        }
        self.insert(id, tile_pos, tile_dir);
        true
    }

    /// Remove tile at the given position.
    pub fn remove(&mut self, pos: Coordinate) {
        debug!("remove tile at pos: {}", pos);

        let count = self.tiles.len();
        self.tiles.retain(|tile| tile.pos != pos);

        if self.tiles.len() != count {
            // update position for next tile append
            self.active_pos = pos;
            self.active_dir = Self::default_active_dir();
            for &dir in Direction::iter() {
                let neighbor_pos = pos.neighbor(dir);
                if let Some(tile) = self.get(neighbor_pos) {
                    let conn = tile.connection(dir.opposite());
                    if conn != Connection::None {
                        self.active_dir = dir;
                        break;
                    }
                }
            }
            debug!("next active pos: {}, dir: {}", self.active_pos, self.active_dir);
        }
    }

    /// Adds a token to the tile at the given position.
    pub fn add_tile_token(&mut self, pos: Coordinate, token: PlacedToken) {
        if let Some(tile) = self.get_mut(pos) {
            tile.tokens.push(token);
        };
    }

    /// Updates all tokens of the tile at the given position.
    pub fn update_tile_tokens(&mut self, pos: Coordinate, tokens: &[PlacedToken]) {
        if let Some(tile) = self.get_mut(pos) {
            tile.tokens = tokens.to_vec();
        }
    }

    /// Re-align all tiles around the map center.
    pub fn align_center(&mut self) {
        if self.tiles.is_empty() {
            return;
        }
        let layout = Layout::default();
        let mut min = (f32::MAX, f32::MAX);
        let mut max = (f32::MIN, f32::MIN);
        for tile in self.tiles.iter() {
            let p = tile.pos.to_pixel(&layout);
            min.0 = min.0.min(p.x());
            min.1 = min.1.min(p.y());
            max.0 = max.0.max(p.x());
            max.1 = max.1.max(p.y());
        }
        let center_x = min.0 + (max.0 - min.0) / 2.0;
        let center_y = min.1 + (max.1 - min.1) / 2.0;
        let center = Coordinate::from_pixel_rounded(&layout, Point(center_x, center_y));
        for tile in self.tiles.iter_mut() {
            tile.pos = tile.pos - center;
        }
        self.active_pos = self.active_pos - center;
    }

    /// Rotate all tile positions to the left (counter-clockwise).
    pub fn rotate_left(&mut self) {
        for tile in self.tiles.iter_mut() {
            tile.pos = tile.pos.rotated_left();
            tile.dir = tile.dir.rotated_left();
        }
        self.active_pos = self.active_pos.rotated_left();
        self.active_dir = self.active_dir.rotated_left();
    }

    /// Rotate all tile positions to the right (clockwise).
    pub fn rotate_right(&mut self) {
        for tile in self.tiles.iter_mut() {
            tile.pos = tile.pos.rotated_right();
            tile.dir = tile.dir.rotated_right();
        }
        self.active_pos = self.active_pos.rotated_right();
        self.active_dir = self.active_dir.rotated_right();
    }
}

impl Default for Map {
    fn default() -> Self {
        Map::new()
    }
}

//----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use approx::assert_abs_diff_eq;
    use indoc::indoc;
    use crate::tile;
    use super::*;

    pub const EPSILON: f32 = 1e-5;

    #[test]
    fn placed_tile_connection() {
        let tile = PlacedTile::new(tile!(0), (0, 0).into(), Direction::A);
        assert_eq!(tile.connection(Direction::A), Connection::None);
        assert_eq!(tile.connection(Direction::B), Connection::None);
        assert_eq!(tile.connection(Direction::C), Connection::None);
        assert_eq!(tile.connection(Direction::D), Connection::None);
        assert_eq!(tile.connection(Direction::E), Connection::None);
        assert_eq!(tile.connection(Direction::F), Connection::None);

        let tile = PlacedTile::new(tile!(102, a), (0, 0).into(), Direction::A);
        assert_eq!(tile.connection(Direction::A), Connection::Straight(0));
        assert_eq!(tile.connection(Direction::B), Connection::None);
        assert_eq!(tile.connection(Direction::C), Connection::None);
        assert_eq!(tile.connection(Direction::D), Connection::Straight(0));
        assert_eq!(tile.connection(Direction::E), Connection::None);
        assert_eq!(tile.connection(Direction::F), Connection::None);

        let tile = PlacedTile::new(tile!(103, a), (0, 0).into(), Direction::B);
        assert_eq!(tile.connection(Direction::A), Connection::None);
        assert_eq!(tile.connection(Direction::B), Connection::Straight(0));
        assert_eq!(tile.connection(Direction::C), Connection::None);
        assert_eq!(tile.connection(Direction::D), Connection::None);
        assert_eq!(tile.connection(Direction::E), Connection::Straight(0));
        assert_eq!(tile.connection(Direction::F), Connection::None);

        let tile = PlacedTile::new(tile!(105, a), (0, 0).into(), Direction::C);
        assert_eq!(tile.connection(Direction::A), Connection::None);
        assert_eq!(tile.connection(Direction::B), Connection::None);
        assert_eq!(tile.connection(Direction::C), Connection::None);
        assert_eq!(tile.connection(Direction::D), Connection::Left(1));
        assert_eq!(tile.connection(Direction::E), Connection::None);
        assert_eq!(tile.connection(Direction::F), Connection::Right(1));
    }

    #[test]
    fn placed_tile_connection_target() {
        let tile = PlacedTile::new(tile!(0), (0, 0).into(), Direction::A);
        assert_eq!(tile.connection_target(Direction::A), None);
        assert_eq!(tile.connection_target(Direction::B), None);
        assert_eq!(tile.connection_target(Direction::C), None);
        assert_eq!(tile.connection_target(Direction::D), None);
        assert_eq!(tile.connection_target(Direction::E), None);
        assert_eq!(tile.connection_target(Direction::F), None);

        let tile = PlacedTile::new(tile!(102, a), (0, 0).into(), Direction::A);
        assert_eq!(tile.connection_target(Direction::A), Some(Direction::D));
        assert_eq!(tile.connection_target(Direction::B), None);
        assert_eq!(tile.connection_target(Direction::C), None);
        assert_eq!(tile.connection_target(Direction::D), Some(Direction::A));
        assert_eq!(tile.connection_target(Direction::E), None);
        assert_eq!(tile.connection_target(Direction::F), None);

        let tile = PlacedTile::new(tile!(103, a), (0, 0).into(), Direction::B);
        assert_eq!(tile.connection_target(Direction::A), None);
        assert_eq!(tile.connection_target(Direction::B), Some(Direction::E));
        assert_eq!(tile.connection_target(Direction::C), None);
        assert_eq!(tile.connection_target(Direction::D), None);
        assert_eq!(tile.connection_target(Direction::E), Some(Direction::B));
        assert_eq!(tile.connection_target(Direction::F), None);

        let tile = PlacedTile::new(tile!(105, a), (0, 0).into(), Direction::C);
        assert_eq!(tile.connection_target(Direction::A), None);
        assert_eq!(tile.connection_target(Direction::B), None);
        assert_eq!(tile.connection_target(Direction::C), None);
        assert_eq!(tile.connection_target(Direction::D), Some(Direction::F));
        assert_eq!(tile.connection_target(Direction::E), None);
        assert_eq!(tile.connection_target(Direction::F), Some(Direction::D));
    }

    #[test]
    fn placed_tile_edge() {
        let tile = PlacedTile::new(tile!(0), (0, 0).into(), Direction::A);
        assert_eq!(tile.edge(Direction::A), Edge::None);
        assert_eq!(tile.edge(Direction::B), Edge::None);
        assert_eq!(tile.edge(Direction::C), Edge::None);
        assert_eq!(tile.edge(Direction::D), Edge::None);
        assert_eq!(tile.edge(Direction::E), Edge::None);
        assert_eq!(tile.edge(Direction::F), Edge::None);

        let tile = PlacedTile::new(tile!(102, a), (0, 0).into(), Direction::A);
        assert_eq!(tile.edge(Direction::A), Edge::Straight(3));
        assert_eq!(tile.edge(Direction::B), Edge::None);
        assert_eq!(tile.edge(Direction::C), Edge::None);
        assert_eq!(tile.edge(Direction::D), Edge::Straight(3));
        assert_eq!(tile.edge(Direction::E), Edge::None);
        assert_eq!(tile.edge(Direction::F), Edge::None);

        let tile = PlacedTile::new(tile!(103, b), (0, 0).into(), Direction::B);
        assert_eq!(tile.edge(Direction::A), Edge::None);
        assert_eq!(tile.edge(Direction::B), Edge::Straight(2));
        assert_eq!(tile.edge(Direction::C), Edge::None);
        assert_eq!(tile.edge(Direction::D), Edge::None);
        assert_eq!(tile.edge(Direction::E), Edge::Straight(2));
        assert_eq!(tile.edge(Direction::F), Edge::None);

        let tile = PlacedTile::new(tile!(124, a), (0, 0).into(), Direction::C);
        assert_eq!(tile.edge(Direction::A), Edge::None);
        assert_eq!(tile.edge(Direction::B), Edge::None);
        assert_eq!(tile.edge(Direction::C), Edge::Straight(3));
        assert_eq!(tile.edge(Direction::D), Edge::None);
        assert_eq!(tile.edge(Direction::E), Edge::SkewLeft(3));
        assert_eq!(tile.edge(Direction::F), Edge::None);
    }

    #[test]
    fn placed_tile_serde() {
        let tile = PlacedTile::new(tile!(0), (0, 0).into(), Direction::A);
        let text = serde_json::to_string(&tile).unwrap();
        assert_eq!(text, r#"{"id":"0","q":0,"r":0,"dir":0}"#);

        let tile = PlacedTile::new(tile!(102, a), (0, 1).into(), Direction::A);
        let text = serde_json::to_string(&tile).unwrap();
        assert_eq!(text, r#"{"id":"102a","q":0,"r":1,"dir":0}"#);

        let tile = PlacedTile::new(tile!(103, b, 1), (1, -2).into(), Direction::B);
        let text = serde_json::to_string(&tile).unwrap();
        assert_eq!(text, r#"{"id":"103b-1","q":1,"r":-2,"dir":1}"#);

        let tile = PlacedTile::new(tile!(124, a), (2, 0).into(), Direction::C);
        let text = serde_json::to_string(&tile).unwrap();
        assert_eq!(text, r#"{"id":"124a","q":2,"r":0,"dir":2}"#);

        let mut tokens = Vec::new();
        tokens.push(PlacedToken::new(TokenId::ChicaneWithLimit(Terrain::Gravel), (0.0, 0.0).into(), 3.0.into()));
        tokens.push(PlacedToken::new(TokenId::Chicane(Terrain::Gravel), (1.0, 0.5).into(), 0.0.into()));
        let tile = PlacedTile::with_tokens(tile!(205, a), (2, -1).into(), Direction::F, tokens);
        let text = serde_json::to_string(&tile).unwrap();
        assert_eq!(text, r#"{"id":"205a","q":2,"r":-1,"dir":5,"tokens":[{"id":"chicane-limit-gravel","q":0.0,"r":0.0,"dir":3.0},{"id":"chicane-gravel","q":1.0,"r":0.5,"dir":0.0}]}"#);

        let text = r#"{"id":"0","q":0,"r":0,"dir":0}"#;
        let tile: PlacedTile = serde_json::from_str(text).unwrap();
        assert_abs_diff_eq!(tile, PlacedTile::new(tile!(0), (0, 0).into(), Direction::A));

        let text = r#"{"id":"102a","q":0,"r":1,"dir":0}"#;
        let tile: PlacedTile = serde_json::from_str(text).unwrap();
        assert_abs_diff_eq!(tile, PlacedTile::new(tile!(102, a), (0, 1).into(), Direction::A));

        let text = r#"{"id": "103b-1", "q": 1, "r": -2, "dir": 1}"#;
        let tile: PlacedTile = serde_json::from_str(text).unwrap();
        assert_abs_diff_eq!(tile, PlacedTile::new(tile!(103, b, 1), (1, -2).into(), Direction::B));

        let text = indoc!(r#"{
            "id": "205a",
            "q": 2,
            "r": -1,
            "dir": 5,
            "tokens": [
                {
                    "id": "chicane-limit-gravel",
                    "q": 0.0,
                    "r": 0.0,
                    "dir": 3.0
                },
                {
                    "id": "chicane-gravel",
                    "q": 1.0,
                    "r": 0.5,
                    "dir": 0.0
                }
            ]
        }"#);
        let tile: PlacedTile = serde_json::from_str(text).unwrap();
        assert_abs_diff_eq!(tile, PlacedTile::with_tokens(tile!(205, a), (2, -1).into(), Direction::F, vec![
            PlacedToken::new(TokenId::ChicaneWithLimit(Terrain::Gravel), (0.0, 0.0).into(), FloatDirection(3.0)),
            PlacedToken::new(TokenId::Chicane(Terrain::Gravel), (1.0, 0.5).into(), FloatDirection(0.0)),
        ]));

        let text = r#"{}"#;
        let result: Result<PlacedTile, _> = serde_json::from_str(text);
        assert!(result.is_err());

        let text = r#"{"id":"102a","q":0,"r":1}}"#;
        let result: Result<TileId, _> = serde_json::from_str(text);
        assert!(result.is_err());
    }

    #[test]
    fn map_insert_and_append() {
        let mut map = Map::new();
        map.set_title("Short Track 2");

        map.insert(tile!(102, b), (0, 0).into(), Direction::D);
        assert_eq!(map.active_pos(), Some(Coordinate::new(1, 0)));
        assert_eq!(map.active_dir(), Direction::D);
        map.append(tile!(104, b), Some(Coordinate::new(-1, 0)), None);
        map.append(tile!(113, b), None, "R".parse().ok());
        map.append(tile!(117, b), None, "r".parse().ok());
        map.append(tile!(114, b), None, "R".parse().ok());
        map.append(tile!(115, b), None, None);
        map.append(tile!(115, b), None, "l".parse().ok());
        map.append(tile!(108, b), None, "r".parse().ok());
        map.append(tile!(110, b), None, "R".parse().ok());
        map.append(tile!(107, b), None, None);
        assert_eq!(map.active_pos(), None);
        map.insert(tile!(101), (0, -2).into(), Direction::A);
        assert_eq!(map.active_pos(), None);

        assert_eq!(map.title(), "Short Track 2");
        assert_abs_diff_eq!(map.tiles(), &[
            PlacedTile::new(tile!(102, b, 0), ( 0,  0).into(), Direction::D),
            PlacedTile::new(tile!(104, b, 1), (-1,  0).into(), Direction::A),
            PlacedTile::new(tile!(113, b, 0), (-2,  0).into(), Direction::D),
            PlacedTile::new(tile!(117, b, 1), (-2, -1).into(), Direction::E),
            PlacedTile::new(tile!(114, b, 0), (-1, -2).into(), Direction::F),
            PlacedTile::new(tile!(115, b, 1), (-1, -1).into(), Direction::D),
            PlacedTile::new(tile!(115, b, 2), ( 0, -1).into(), Direction::C),
            PlacedTile::new(tile!(108, b, 0), ( 1, -2).into(), Direction::F),
            PlacedTile::new(tile!(110, b, 0), ( 1, -1).into(), Direction::B),
            PlacedTile::new(tile!(107, b, 1), ( 1,  0).into(), Direction::B),
            PlacedTile::new(tile!(101),       ( 0, -2).into(), Direction::A),
        ][..]);
    }

    #[test]
    fn map_append_pos_none() {
        let mut map = Map::new();
        assert_eq!(map.active_pos(), Some(Coordinate::new(0, 0)));
        assert_eq!(map.active_dir(), Direction::D);

        map.set_active_pos((1, 0).into());
        assert_eq!(map.active_pos(), Some(Coordinate::new(1, 0)));
        assert_eq!(map.active_dir(), Direction::D);

        map.append(tile!(105, b), None, Some(ConnectionHint::Right));
        let tile = map.get((1, 0).into()).unwrap();
        assert_eq!(tile.id(), tile!(105, b, 1));
        assert_eq!(tile.dir, Direction::A);
        assert_eq!(map.active_pos(), Some(Coordinate::new(1, 1)));
        assert_eq!(map.active_dir(), Direction::E);

        map.set_active_pos((0, 0).into());
        assert_eq!(map.active_pos(), Some(Coordinate::new(0, 0)));
        assert_eq!(map.active_dir(), Direction::A);

        map.append(tile!(105, b), None, Some(ConnectionHint::Right));
        let tile = map.get((0, 0).into()).unwrap();
        assert_eq!(tile.id(), tile!(105, b, 2));
        assert_eq!(tile.dir, Direction::D);
        assert_eq!(map.active_pos(), Some(Coordinate::new(0, -1)));
        assert_eq!(map.active_dir(), Direction::B);
    }

    #[test]
    fn map_append_dir_none() {
        let mut map = Map::new();
        assert_eq!(map.active_pos(), Some(Coordinate::new(0, 0)));
        assert_eq!(map.active_dir(), Direction::D);

        map.append(tile!(105, b), None, None);
        let tile = map.get((0, 0).into()).unwrap();
        assert_eq!(tile.id(), tile!(105, b, 1));
        assert_eq!(tile.dir, Direction::A);
        assert_eq!(map.active_pos(), Some(Coordinate::new(0, 1)));
        assert_eq!(map.active_dir(), Direction::E);
    }

    #[test]
    fn map_append_dir_left() {
        let mut map = Map::new();
        assert_eq!(map.active_pos(), Some(Coordinate::new(0, 0)));
        assert_eq!(map.active_dir(), Direction::D);

        map.append(tile!(105, b), None, Some(ConnectionHint::Left));
        let tile = map.get((0, 0).into()).unwrap();
        assert_eq!(tile.id(), tile!(105, b, 1));
        assert_eq!(tile.dir, Direction::C);
        assert_eq!(map.active_pos(), Some(Coordinate::new(1, -1)));
        assert_eq!(map.active_dir(), Direction::C);
    }

    #[test]
    fn map_append_dir_right() {
        let mut map = Map::new();
        assert_eq!(map.active_pos(), Some(Coordinate::new(0, 0)));
        assert_eq!(map.active_dir(), Direction::D);

        map.append(tile!(105, b), None, Some(ConnectionHint::Right));
        let tile = map.get((0, 0).into()).unwrap();
        assert_eq!(tile.id(), tile!(105, b, 1));
        assert_eq!(tile.dir, Direction::A);
        assert_eq!(map.active_pos(), Some(Coordinate::new(0, 1)));
        assert_eq!(map.active_dir(), Direction::E);
    }

    #[test]
    fn map_append_dir_straight() {
        let mut map = Map::new();
        assert_eq!(map.active_pos(), Some(Coordinate::new(0, 0)));
        assert_eq!(map.active_dir(), Direction::D);

        map.append(tile!(105, b), None, Some(ConnectionHint::Straight));
        let tile = map.get((0, 0).into()).unwrap();
        assert_eq!(tile.id(), tile!(105, b, 1));
        assert_eq!(tile.dir, Direction::A);
        assert_eq!(map.active_pos(), Some(Coordinate::new(0, 1)));
        assert_eq!(map.active_dir(), Direction::E);
    }

    #[test]
    fn map_append_rotate1() {
        let mut map = Map::new();
        map.insert(tile!(103, b), (-1,  0).into(), Direction::F);
        map.insert(tile!(105, b), ( 0, -1).into(), Direction::F);
        map.insert(tile!(102, b), ( 1, -1).into(), Direction::A);
        map.insert(tile!(108, b), ( 2, -1).into(), Direction::A);
        map.insert(tile!(105, b), ( 1,  0).into(), Direction::C);
        // append should choose direction such that track does not end on any
        // surrounding tile edge
        map.append(tile!(107, b), None, None);
        let tile = map.get((0, 0).into()).unwrap();
        assert_eq!(tile.id(), tile!(107, b, 1));
        assert_eq!(tile.dir, Direction::E);
    }

    #[test]
    fn map_append_rotate2() {
        let mut map = Map::new();
        map.insert(tile!(103, a), ( 1,  0).into(), Direction::B);
        map.insert(tile!(105, a), ( 1, -1).into(), Direction::B);
        map.insert(tile!(102, a), ( 0, -1).into(), Direction::D);
        map.insert(tile!(108, a), (-1, -1).into(), Direction::E);
        map.insert(tile!(105, a), (-1,  0).into(), Direction::D);
        // append should choose direction such that track does not end on any
        // surrounding tile edge
        map.append(tile!(107, a), None, None);
        let tile = map.get((0, 0).into()).unwrap();
        assert_eq!(tile.id(), tile!(107, a, 1));
        assert_eq!(tile.dir, Direction::A);
    }

    #[test]
    fn map_align_center() {
        let mut map = Map::new();
        map.align_center();
        let mut tiles = map.tiles().iter();
        assert_eq!(tiles.next(), None);

        let mut map = Map::new();
        map.insert(tile!(102, b, 0), (6, -1).into(), Direction::E);
        map.insert(tile!(104, b, 1), (6,  0).into(), Direction::B);
        map.insert(tile!(113, b, 0), (6,  1).into(), Direction::E);
        map.insert(tile!(117, b, 1), (5,  2).into(), Direction::F);
        map.insert(tile!(114, b, 0), (4,  2).into(), Direction::A);
        map.insert(tile!(115, b, 1), (5,  1).into(), Direction::E);
        map.insert(tile!(115, b, 2), (5,  0).into(), Direction::D);
        map.insert(tile!(108, b, 0), (4,  0).into(), Direction::A);
        map.insert(tile!(110, b, 0), (5, -1).into(), Direction::C);
        map.insert(tile!(107, b, 1), (6, -2).into(), Direction::C);
        map.insert(tile!(101, a, 0), (4,  1).into(), Direction::E);
        map.align_center();
        assert_abs_diff_eq!(map.tiles(), &[
            PlacedTile::new(tile!(102, b, 0), ( 1, -1).into(), Direction::E),
            PlacedTile::new(tile!(104, b, 1), ( 1,  0).into(), Direction::B),
            PlacedTile::new(tile!(113, b, 0), ( 1,  1).into(), Direction::E),
            PlacedTile::new(tile!(117, b, 1), ( 0,  2).into(), Direction::F),
            PlacedTile::new(tile!(114, b, 0), (-1,  2).into(), Direction::A),
            PlacedTile::new(tile!(115, b, 1), ( 0,  1).into(), Direction::E),
            PlacedTile::new(tile!(115, b, 2), ( 0,  0).into(), Direction::D),
            PlacedTile::new(tile!(108, b, 0), (-1,  0).into(), Direction::A),
            PlacedTile::new(tile!(110, b, 0), ( 0, -1).into(), Direction::C),
            PlacedTile::new(tile!(107, b, 1), ( 1, -2).into(), Direction::C),
            PlacedTile::new(tile!(101, a, 0), (-1,  1).into(), Direction::E),
        ][..]);
    }

    #[test]
    fn map_rotate_left() {
        let mut map = Map::new();
        map.insert(tile!(102, b, 0), ( 1, -1).into(), Direction::E);
        map.insert(tile!(104, b, 1), ( 1,  0).into(), Direction::B);
        map.insert(tile!(113, b, 0), ( 1,  1).into(), Direction::E);
        map.insert(tile!(117, b, 1), ( 0,  2).into(), Direction::F);
        map.insert(tile!(114, b, 0), (-1,  2).into(), Direction::A);
        map.insert(tile!(115, b, 1), ( 0,  1).into(), Direction::E);
        map.insert(tile!(115, b, 2), ( 0,  0).into(), Direction::D);
        map.insert(tile!(108, b, 0), (-1,  0).into(), Direction::A);
        map.insert(tile!(110, b, 0), ( 0, -1).into(), Direction::C);
        map.insert(tile!(107, b, 1), ( 1, -2).into(), Direction::C);
        map.insert(tile!(101, a, 0), (-1,  1).into(), Direction::E);
        map.rotate_left();
        assert_abs_diff_eq!(map.tiles(), &[
            PlacedTile::new(tile!(102, b, 0), ( 0, -1).into(), Direction::D),
            PlacedTile::new(tile!(104, b, 1), ( 1, -1).into(), Direction::A),
            PlacedTile::new(tile!(113, b, 0), ( 2, -1).into(), Direction::D),
            PlacedTile::new(tile!(117, b, 1), ( 2,  0).into(), Direction::E),
            PlacedTile::new(tile!(114, b, 0), ( 1,  1).into(), Direction::F),
            PlacedTile::new(tile!(115, b, 1), ( 1,  0).into(), Direction::D),
            PlacedTile::new(tile!(115, b, 2), ( 0,  0).into(), Direction::C),
            PlacedTile::new(tile!(108, b, 0), (-1,  1).into(), Direction::F),
            PlacedTile::new(tile!(110, b, 0), (-1,  0).into(), Direction::B),
            PlacedTile::new(tile!(107, b, 1), (-1, -1).into(), Direction::B),
            PlacedTile::new(tile!(101, a, 0), ( 0,  1).into(), Direction::D),
        ][..]);
    }

    #[test]
    fn map_rotate_right() {
        let mut map = Map::new();
        map.insert(tile!(102, b, 0), ( 1, -1).into(), Direction::E);
        map.insert(tile!(104, b, 1), ( 1,  0).into(), Direction::B);
        map.insert(tile!(113, b, 0), ( 1,  1).into(), Direction::E);
        map.insert(tile!(117, b, 1), ( 0,  2).into(), Direction::F);
        map.insert(tile!(114, b, 0), (-1,  2).into(), Direction::A);
        map.insert(tile!(115, b, 1), ( 0,  1).into(), Direction::E);
        map.insert(tile!(115, b, 2), ( 0,  0).into(), Direction::D);
        map.insert(tile!(108, b, 0), (-1,  0).into(), Direction::A);
        map.insert(tile!(110, b, 0), ( 0, -1).into(), Direction::C);
        map.insert(tile!(107, b, 1), ( 1, -2).into(), Direction::C);
        map.insert(tile!(101, a, 0), (-1,  1).into(), Direction::E);
        map.rotate_right();
        assert_abs_diff_eq!(map.tiles(), &[
            PlacedTile::new(tile!(102, b, 0), ( 1,  0).into(), Direction::F),
            PlacedTile::new(tile!(104, b, 1), ( 0,  1).into(), Direction::C),
            PlacedTile::new(tile!(113, b, 0), (-1,  2).into(), Direction::F),
            PlacedTile::new(tile!(117, b, 1), (-2,  2).into(), Direction::A),
            PlacedTile::new(tile!(114, b, 0), (-2,  1).into(), Direction::B),
            PlacedTile::new(tile!(115, b, 1), (-1,  1).into(), Direction::F),
            PlacedTile::new(tile!(115, b, 2), ( 0,  0).into(), Direction::E),
            PlacedTile::new(tile!(108, b, 0), ( 0, -1).into(), Direction::B),
            PlacedTile::new(tile!(110, b, 0), ( 1, -1).into(), Direction::D),
            PlacedTile::new(tile!(107, b, 1), ( 2, -1).into(), Direction::D),
            PlacedTile::new(tile!(101, a, 0), (-1,  0).into(), Direction::F),
        ][..]);
    }

    #[test]
    fn map_serde() {
        let map = Map::new();
        let text = serde_json::to_string_pretty(&map).unwrap();
        assert_eq!(text, indoc!(r#"{
          "title": "My Track",
          "tiles": [],
          "active_pos": {
            "q": 0,
            "r": 0
          },
          "active_dir": 3
        }"#));

        let mut map = Map::new();
        map.set_title("Short Track 2");
        map.insert(tile!(102, b, 0), ( 1, -1).into(), Direction::E);
        map.insert(tile!(104, b, 1), ( 1,  0).into(), Direction::B);
        map.insert(tile!(113, b, 0), ( 1,  1).into(), Direction::E);
        map.insert(tile!(117, b, 1), ( 0,  2).into(), Direction::F);
        map.insert(tile!(114, b, 0), (-1,  2).into(), Direction::A);
        map.insert(tile!(115, b, 1), ( 0,  1).into(), Direction::E);
        map.insert(tile!(115, b, 2), ( 0,  0).into(), Direction::D);
        map.insert(tile!(108, b, 0), (-1,  0).into(), Direction::A);
        map.insert(tile!(110, b, 0), ( 0, -1).into(), Direction::C);
        map.insert(tile!(107, b, 1), ( 1, -2).into(), Direction::C);
        map.insert(tile!(101, a, 0), (-1,  1).into(), Direction::E);
        let text = serde_json::to_string_pretty(&map).unwrap();
        assert_eq!(text, indoc!(r#"{
          "title": "Short Track 2",
          "tiles": [
            {
              "id": "102b",
              "q": 1,
              "r": -1,
              "dir": 4
            },
            {
              "id": "104b-1",
              "q": 1,
              "r": 0,
              "dir": 1
            },
            {
              "id": "113b",
              "q": 1,
              "r": 1,
              "dir": 4
            },
            {
              "id": "117b-1",
              "q": 0,
              "r": 2,
              "dir": 5
            },
            {
              "id": "114b",
              "q": -1,
              "r": 2,
              "dir": 0
            },
            {
              "id": "115b-1",
              "q": 0,
              "r": 1,
              "dir": 4
            },
            {
              "id": "115b-2",
              "q": 0,
              "r": 0,
              "dir": 3
            },
            {
              "id": "108b",
              "q": -1,
              "r": 0,
              "dir": 0
            },
            {
              "id": "110b",
              "q": 0,
              "r": -1,
              "dir": 2
            },
            {
              "id": "107b-1",
              "q": 1,
              "r": -2,
              "dir": 2
            },
            {
              "id": "101a",
              "q": -1,
              "r": 1,
              "dir": 4
            }
          ],
          "active_pos": {
            "q": -1,
            "r": 1
          },
          "active_dir": 3
        }"#));

        let text = indoc!(r#"{
            "title": "Short Track 2",
            "tiles": [
                {"id": "102b",   "q":  1, "r": -1, "dir": 4},
                {"id": "104b-1", "q":  1, "r":  0, "dir": 1},
                {"id": "113b",   "q":  1, "r":  1, "dir": 4},
                {"id": "117b-1", "q":  0, "r":  2, "dir": 5},
                {"id": "114b",   "q": -1, "r":  2, "dir": 0},
                {"id": "115b-1", "q":  0, "r":  1, "dir": 4},
                {"id": "115b-2", "q":  0, "r":  0, "dir": 3},
                {"id": "108b",   "q": -1, "r":  0, "dir": 0},
                {"id": "110b",   "q":  0, "r": -1, "dir": 2},
                {"id": "107b-1", "q":  1, "r": -2, "dir": 2},
                {"id": "101a",   "q": -1, "r":  1, "dir": 4}
            ]
        }"#);
        let map: Map = serde_json::from_str(text).unwrap();
        assert_eq!(map.title(), "Short Track 2");
        assert_abs_diff_eq!(map.tiles(), &[
            PlacedTile::new(tile!(102, b, 0), ( 1, -1).into(), Direction::E),
            PlacedTile::new(tile!(104, b, 1), ( 1,  0).into(), Direction::B),
            PlacedTile::new(tile!(113, b, 0), ( 1,  1).into(), Direction::E),
            PlacedTile::new(tile!(117, b, 1), ( 0,  2).into(), Direction::F),
            PlacedTile::new(tile!(114, b, 0), (-1,  2).into(), Direction::A),
            PlacedTile::new(tile!(115, b, 1), ( 0,  1).into(), Direction::E),
            PlacedTile::new(tile!(115, b, 2), ( 0,  0).into(), Direction::D),
            PlacedTile::new(tile!(108, b, 0), (-1,  0).into(), Direction::A),
            PlacedTile::new(tile!(110, b, 0), ( 0, -1).into(), Direction::C),
            PlacedTile::new(tile!(107, b, 1), ( 1, -2).into(), Direction::C),
            PlacedTile::new(tile!(101, a, 0), (-1,  1).into(), Direction::E),
        ][..]);
        assert_eq!(map.active_pos(), None);
    }
}

//----------------------------------------------------------------------------
