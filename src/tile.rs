//----------------------------------------------------------------------------
//! Tile identifier and characteristics, map of tiles within a hexagon grid.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

use serde::{Serialize, Serializer, Deserialize, Deserializer};
use serde::de::{self, Visitor};

use crate::hexagon::{Coordinate, Direction, Layout, Point};

//----------------------------------------------------------------------------

/// Identifier of a game tile.
///
/// Each tile identifier consists of three parts:
/// * a three digit catalog number `num` (101 .. 999)
/// * a number `side` for the tile front (a) or back (b) (1 => a, 2 => b)
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

impl FromStr for TileId {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
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

impl Serialize for TileId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for TileId {
    fn deserialize<D>(deserializer: D) -> Result<TileId, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct TileVisitor;

        impl<'de> Visitor<'de> for TileVisitor {
            type Value = TileId;

            fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
                fmt.write_str("a tile identifier string")
            }

            fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
            where
                E: de::Error,
            {
                match TileId::from_str(value) {
                    Ok(val) => Ok(val),
                    Err(_) => Err(E::custom(format!("invalid tile identifier: {}", value))),
                }
            }
        }

        deserializer.deserialize_string(TileVisitor)
    }
}

/// Creates a new tile identifier.
///
/// The `tile!` macro adds convenience on directly calling `TileId::new()` as it
/// allows to skip `side` or `var` parameters, if unused. Also the value for
/// parameter `side` is restricted to `a` and `b` to avoid invalid tile
/// identifiers.
///
/// Examples:
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

/// Single tile that is part of the map. A placed tile consists of tile
/// identifier, grid coordinates, and rotation direction.
///
/// Contains some private map helper functions that apply tile information
/// to the placed tile.
#[derive(Clone, Serialize)]
pub struct PlacedTile {
    id: TileId,
    #[serde(flatten)]
    pub pos: Coordinate,
    pub dir: Direction,
    #[serde(skip_serializing)]
    info: Option<&'static TileInfo>,
}

impl PlacedTile {
    /// Creates a new tile with identifier, coordinates, and rotation.
    pub fn new(id: TileId, pos: Coordinate, dir: Direction) -> Self {
        let info = TileInfo::get(id);
        PlacedTile { id, pos, dir, info }
    }

    /// Tile identifier.
    pub fn id(&self) -> TileId {
        self.id
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
            // skip printing cached tile info
            .finish()
    }
}

impl PartialEq<PlacedTile> for PlacedTile {
    fn eq(&self, other: &PlacedTile) -> bool {
        // skip comparing cached tile info reference
        self.id == other.id && self.pos == other.pos && self.dir == other.dir
    }
}

impl<'de> Deserialize<'de> for PlacedTile {
    fn deserialize<D>(deserializer: D) -> Result<PlacedTile, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "lowercase")]
        enum Field { Id, Q, R, Dir }

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
                Ok(PlacedTile::new(id, (q, r).into(), dir))
            }

            fn visit_map<V>(self, mut map: V) -> Result<PlacedTile, V::Error>
            where
                V: de::MapAccess<'de>,
            {
                let mut id = None;
                let mut q = None;
                let mut r = None;
                let mut dir = None;
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
                    }
                }
                let id = id.ok_or_else(|| de::Error::missing_field("id"))?;
                let q = q.ok_or_else(|| de::Error::missing_field("q"))?;
                let r = r.ok_or_else(|| de::Error::missing_field("r"))?;
                let dir = dir.ok_or_else(|| de::Error::missing_field("dir"))?;
                Ok(PlacedTile::new(id, (q, r).into(), dir))
            }
        }

        const FIELDS: [&str; 4] = ["id", "q", "r", "dir"];
        deserializer.deserialize_struct("tile", &FIELDS, PlacedTileVisitor)
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
        let tiles = Vec::new();
        let title = "My Track".to_string();
        let active_pos = Coordinate::default();
        let active_dir = Self::default_active_dir();
        Map { tiles, title, active_pos, active_dir }
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
        self.tiles.iter().find(|tile| tile.pos == pos)
    }

    /// Insert a new tile using the specified position and direction.
    pub fn insert(&mut self, id: TileId, pos: Coordinate, dir: Direction) {
        // remove existing tile at insert position
        self.tiles.retain(|tile| tile.pos != pos);

        // auto-select tile id variant, if not given
        let mut id = id;
        if id.var() == 0 {
            let count = self.tiles.iter()
                .filter(|tile| tile.id.base() == id).count();
            match TileInfo::get(id) {
                Some(info) if info.count > 1 => {
                    id.var = ((count % info.count) + 1) as u8;
                },
                _ => (),
            }
        }

        debug!("insert of tile {} at pos: {}, dir: {}", id, pos, dir);
        let tile = PlacedTile::new(id, pos, dir);
        self.tiles.push(tile);

        // update active tile append position
        self.set_active_pos(pos);
    }

    /// Append a new tile to the map using the active position. In contrast
    /// to `insert()` this function allows auto-detection of tile position and
    /// direction.
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
                    let conn = info.conn[self.active_dir - dir];
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

//----------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone, Copy, PartialEq)]
enum Connection {
    None,
    Straight(i8),
    Left(i8),
    Right(i8),
    JunctionLeft(i8, i8),
    JunctionRight(i8, i8),
}

impl Connection {
    fn target(&self, source: Direction) -> Option<Direction> {
        match *self {
            Connection::None =>
                None,
            Connection::Straight(val) =>
                Some(source + (3 + val).into()),
            Connection::Left(val) =>
                Some(source + (3 - val).into()),
            Connection::Right(val) =>
                Some(source + (3 + val).into()),
            Connection::JunctionLeft(val, _) =>
                Some(source + (3 - val).into()),
            Connection::JunctionRight(val, _) =>
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
            Connection::JunctionLeft(_, _) =>
                ConnectionHint::Left,
            Connection::JunctionRight(_, _) =>
                ConnectionHint::Right,
        };
        hint == *other
    }
}

//----------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Edge {
    None,
    Straight(u8),
    SkewLeft(u8),
    SkewRight(u8),
}

impl Edge {
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

#[derive(Debug, Default)]
pub struct TileInfo {
    id: TileId,
    count: usize,
    conn: [Connection; 6],
    edges: [Edge; 6],
}

impl TileInfo {
    const fn new(id: TileId, count: usize, conn: [Connection; 6], edges: [Edge; 6]) -> Self {
        TileInfo { id, count, conn, edges }
    }

    pub fn base_id(&self) -> TileId {
        self.id
    }

    pub fn full_id(&self) -> TileId {
        match self.count {
            1 => TileId::new(self.id.num, self.id.side, 0),
            _ => TileId::new(self.id.num, self.id.side, 1),
        }
    }

    pub fn iter() -> std::slice::Iter<'static, Self> {
        TILE_INFOS.iter()
    }

    pub fn get(id: TileId) -> Option<&'static Self> {
        let idx = TILE_INFOS.binary_search_by_key(&id.base(), |info| info.id);
        match idx {
            Ok(idx) => Some(&TILE_INFOS[idx]),
            Err(_) => None
        }
    }

    pub fn count(&self) -> usize {
        self.count
    }

    fn connection(&self, dir: Direction) -> Connection {
        self.conn[dir]
    }

    fn connection_target(&self, source: Direction) -> Option<Direction> {
        self.conn[source].target(source)
    }

    pub fn edge(&self, dir: Direction) -> Edge {
        self.edges[dir]
    }
}

impl fmt::Display for TileInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.id)
    }
}

//----------------------------------------------------------------------------

const CN: Connection = Connection::None;
const CS0: Connection = Connection::Straight(0);
const CS1P: Connection = Connection::Straight(1);
const CS1M: Connection = Connection::Straight(-1);
const CL0: Connection = Connection::Left(0);
const CL1: Connection = Connection::Left(1);
const CL2: Connection = Connection::Left(2);
const CR0: Connection = Connection::Right(0);
const CR1: Connection = Connection::Right(1);
const CR2: Connection = Connection::Right(2);
const CJS0L1: Connection = Connection::JunctionLeft(0, 1);
const CJS0L2: Connection = Connection::JunctionLeft(0, 2);
const CJS0R1: Connection = Connection::JunctionRight(0, 1);
const CJS0R2: Connection = Connection::JunctionRight(0, 2);
const CJL1R2: Connection = Connection::JunctionLeft(1, -2);
const CJR1L2: Connection = Connection::JunctionRight(1, -2);

const EN: Edge = Edge::None;
const ES2: Edge = Edge::Straight(2);
const ES3: Edge = Edge::Straight(3);
const ES4: Edge = Edge::Straight(4);
const EL2: Edge = Edge::SkewLeft(2);
const EL3: Edge = Edge::SkewLeft(3);
const ER2: Edge = Edge::SkewRight(2);
const ER3: Edge = Edge::SkewRight(3);

const TILE_INFOS: [TileInfo; 109] = [
    TileInfo::new(tile!(101), 1, [CN; 6], [EN; 6]),
    TileInfo::new(tile!(102, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES3, EN, EN, ES3, EN, EN]),
    TileInfo::new(tile!(102, b), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(103, a), 3, [CS0, CN, CN, CS0, CN, CN], [ES3, EN, EN, ES3, EN, EN]),
    TileInfo::new(tile!(103, b), 3, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(104, a), 3, [CS0, CN, CN, CS0, CN, CN], [ES3, EN, EN, ES3, EN, EN]),
    TileInfo::new(tile!(104, b), 3, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(105, a), 2, [CN, CL1, CN, CR1, CN, CN], [EN, ES3, EN, ES3, EN, EN]),
    TileInfo::new(tile!(105, b), 2, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(106, a), 2, [CN, CL1, CN, CR1, CN, CN], [EN, ES3, EN, ES3, EN, EN]),
    TileInfo::new(tile!(106, b), 2, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(107, a), 2, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES3, ES3, EN, EN]),
    TileInfo::new(tile!(107, b), 2, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(108, a), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES3, ES3, EN, EN]),
    TileInfo::new(tile!(108, b), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(109, a), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES3, ES3, EN, EN]),
    TileInfo::new(tile!(109, b), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(110, a), 1, [CL0, CN, CN, CR0, CN, CN], [ES3, EN, EN, ES3, EN, EN]),
    TileInfo::new(tile!(110, b), 1, [CL0, CN, CN, CR0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(111, a), 2, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES3, ES3, EN, EN]),
    TileInfo::new(tile!(111, b), 2, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(112, a), 2, [CN, CL1, CN, CR1, CN, CN], [EN, ES3, EN, ES3, EN, EN]),
    TileInfo::new(tile!(112, b), 2, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(113, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES3, EN, ES3, EN, EN]),
    TileInfo::new(tile!(113, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(114, a), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES3, ES3, EN, EN]),
    TileInfo::new(tile!(114, b), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(115, a), 2, [CN, CL1, CN, CR1, CN, CN], [EN, ES3, EN, ES3, EN, EN]),
    TileInfo::new(tile!(115, b), 2, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(116, a), 2, [CL0, CN, CN, CR0, CN, CN], [ES3, EN, EN, ES3, EN, EN]),
    TileInfo::new(tile!(116, b), 2, [CL0, CN, CN, CR0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(117, a), 2, [CN, CL1, CN, CR1, CN, CN], [EN, ES3, EN, ES3, EN, EN]),
    TileInfo::new(tile!(117, b), 2, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(118, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES3, EN, ES3, EN, EN]),
    TileInfo::new(tile!(118, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(119, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES3, EN, EN]),
    TileInfo::new(tile!(119, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES3, EN, EN]),
    TileInfo::new(tile!(120, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES3, EN, EN]),
    TileInfo::new(tile!(120, b), 1, [CN, CN, CN, CL1, CN, CR1], [EN, EN, EN, ES3, EN, ES2]),
    TileInfo::new(tile!(121, a), 2, [CN, CN, CS1M, CN, CS1P, CN], [EN, EN, EL3, EN, ER3, EN]),
    TileInfo::new(tile!(121, b), 2, [CN, CN, CS1M, CN, CS1P, CN], [EN, EN, EL2, EN, ER2, EN]),
    TileInfo::new(tile!(122, a), 2, [CN, CN, CN, CN, CL2, CR2], [EN, EN, EN, EN, ER3, ES3]),
    TileInfo::new(tile!(122, b), 2, [CN, CN, CN, CN, CL2, CR2], [EN, EN, EN, EN, ER2, ES2]),
    TileInfo::new(tile!(123, a), 1, [CN, CL2, CR2, CN, CN, CN], [EN, ES3, EL3, EN, EN, EN]),
    TileInfo::new(tile!(123, b), 1, [CN, CL2, CR2, CN, CN, CN], [EN, ES2, EL2, EN, EN, EN]),
    TileInfo::new(tile!(124, a), 2, [CL1, CN, CR1, CN, CN, CN], [ES3, EN, EL3, EN, EN, EN]),
    TileInfo::new(tile!(124, b), 2, [CL1, CN, CR1, CN, CN, CN], [ES2, EN, EL2, EN, EN, EN]),
    TileInfo::new(tile!(125, a), 2, [CR1, CN, CN, CN, CL1, CN], [ES3, EN, EN, EN, ER3, EN]),
    TileInfo::new(tile!(125, b), 2, [CR1, CN, CN, CN, CL1, CN], [ES2, EN, EN, EN, ER2, EN]),
    TileInfo::new(tile!(126, a), 1, [CN, CN, CN, CL2, CR2, CN], [EN, EN, EN, ES3, ER3, EN]),
    TileInfo::new(tile!(126, b), 1, [CN, CN, CN, CL2, CR2, CN], [EN, EN, EN, ES2, ER2, EN]),
    TileInfo::new(tile!(127, a), 2, [CN, CN, CL2, CR2, CN, CN], [EN, EN, EL3, ES3, EN, EN]),
    TileInfo::new(tile!(127, b), 2, [CN, CN, CL2, CR2, CN, CN], [EN, EN, EL2, ES2, EN, EN]),
    TileInfo::new(tile!(128, a), 1, [CN, CN, CN, CN, CL2, CR2], [EN, EN, EN, EN, ER3, EL3]),
    TileInfo::new(tile!(128, b), 1, [CN, CN, CN, CN, CL2, CR2], [EN, EN, EN, EN, ER2, EL2]),
    TileInfo::new(tile!(129, a), 1, [CN, CR0, CN, CN, CL0, CN], [EN, ES3, EN, EN, ER3, EN]),
    TileInfo::new(tile!(129, b), 1, [CN, CR0, CN, CN, CL0, CN], [EN, ES2, EN, EN, ER2, EN]),
    TileInfo::new(tile!(130, a), 1, [CN, CN, CR0, CN, CN, CL0], [EN, EN, EL3, EN, EN, ES3]),
    TileInfo::new(tile!(130, b), 1, [CN, CN, CR0, CN, CN, CL0], [EN, EN, EL2, EN, EN, ES2]),
    TileInfo::new(tile!(131, a), 1, [CN, CN, CS1M, CN, CS1P, CN], [EN, EN, EL3, EN, ER3, EN]),
    TileInfo::new(tile!(131, b), 1, [CN, CN, CS1M, CN, CS1P, CN], [EN, EN, EL2, EN, ER2, EN]),
    TileInfo::new(tile!(132, a), 1, [CN, CN, CS1M, CN, CS1P, CN], [EN, EN, EL3, EN, ER3, EN]),
    TileInfo::new(tile!(132, b), 1, [CN, CN, CS1M, CN, CS1P, CN], [EN, EN, EL2, EN, ER2, EN]),
    TileInfo::new(tile!(133, a), 1, [CN, CS1P, CS1M, CN, CS1P, CS1M], [EN, ER3, EL3, EN, ER3, EL3]),
    TileInfo::new(tile!(133, b), 1, [CN, CS1P, CS1M, CN, CS1P, CS1M], [EN, ER2, EL2, EN, ER2, EL2]),
    TileInfo::new(tile!(134, a), 1, [CN, CN, CR0, CN, CN, CR0], [EN, EN, EL3, EN, EN, EL3]),
    TileInfo::new(tile!(134, b), 1, [CN, CN, CR0, CN, CN, CR0], [EN, EN, EL2, EN, EN, EL2]),
    TileInfo::new(tile!(135, a), 1, [CN, CL0, CN, CN, CL0, CN], [EN, ER3, EN, EN, ER3, EN]),
    TileInfo::new(tile!(135, b), 1, [CN, CL0, CN, CN, CL0, CN], [EN, ER2, EN, EN, ER2, EN]),
    TileInfo::new(tile!(136, a), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES3, ES3, EN, EN]),
    TileInfo::new(tile!(136, b), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(137, a), 1, [CN, CN, CL1, CN, CR1, CN], [EN, EN, ES3, EN, ER3, EN]),
    TileInfo::new(tile!(137, b), 1, [CN, CN, CL1, CN, CR1, CN], [EN, EN, ES2, EN, ER2, EN]),
    TileInfo::new(tile!(138, a), 2, [CN, CN, CL1, CN, CR1, CN], [EN, EN, EL3, EN, ES3, EN]),
    TileInfo::new(tile!(138, b), 2, [CN, CN, CL1, CN, CR1, CN], [EN, EN, EL2, EN, ES2, EN]),
    TileInfo::new(tile!(139, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES4, EN, EN, ES4, EN, EN]),
    TileInfo::new(tile!(139, b), 1, [CN, CS1P, CS1M, CN, CS1P, CS1M], [EN, ER3, EL3, EN, ER3, EL3]),
    TileInfo::new(tile!(140, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES4, EN, EN, ES4, EN, EN]),
    TileInfo::new(tile!(140, b), 1, [CN, CS1P, CS1M, CN, CS1P, CS1M], [EN, ER3, EL3, EN, ER3, EL3]),
    TileInfo::new(tile!(141, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES4, EN, EN, ES4, EN, EN]),
    TileInfo::new(tile!(141, b), 1, [CS0, CN, CS0, CS0, CN, CS0], [ES3, EN, ES3, ES3, EN, ES3]),
    TileInfo::new(tile!(142, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES4, EN, EN, ES4, EN, EN]),
    TileInfo::new(tile!(142, b), 1, [CS0, CN, CS1M, CS0, CS1P, CN], [ES3, EN, EL3, ES3, ER3, EN]),
    TileInfo::new(tile!(143, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES4, EN, EN, ES4, EN, EN]),
    TileInfo::new(tile!(143, b), 1, [CN, CS1P, CS1M, CN, CS1P, CS1M], [EN, ER2, EL2, EN, ER2, EL2]),
    TileInfo::new(tile!(144, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES4, EN, EN, ES4, EN, EN]),
    TileInfo::new(tile!(144, b), 1, [CN, CS1P, CS1M, CN, CS1P, CS1M], [EN, ER2, EL2, EN, ER2, EL2]),
    TileInfo::new(tile!(145, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES4, EN, EN, ES4, EN, EN]),
    TileInfo::new(tile!(145, b), 1, [CS0, CN, CS0, CS0, CN, CS0], [ES2, EN, ES2, ES2, EN, ES2]),
    TileInfo::new(tile!(146, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES4, EN, ES4, EN, EN]),
    TileInfo::new(tile!(146, b), 1, [CS0, CN, CS1M, CS0, CS1P, CN], [ES2, EN, EL2, ES2, ER2, EN]),
    TileInfo::new(tile!(147, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES4, EN, ES4, EN, EN]),
    TileInfo::new(tile!(147, b), 1, [CN, CS1P, CN, CL2, CR2, CS1M], [EN, ER3, EN, ES3, ER3, EL3]),
    TileInfo::new(tile!(148, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES4, EN, ES4, EN, EN]),
    TileInfo::new(tile!(148, b), 1, [CN, CS1P, CL2, CR2, CN, CS1M], [EN, ER3, EL3, ES3, EN, EL3]),
    TileInfo::new(tile!(149, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES4, EN, ES4, EN, EN]),
    TileInfo::new(tile!(149, b), 1, [CN, CS1P, CL2, CR2, CN, CS1M], [EN, ER3, ES3, ES3, EN, EL3]),
    TileInfo::new(tile!(150, a), 1, [CS0, CS1P, CS1M, CS0, CS1P, CS1M], [ES3, ER3, EL3, ES3, ER3, EL3]),
    TileInfo::new(tile!(150, b), 1, [CS0, CS1P, CS1M, CS0, CS1P, CS1M], [ES2, ER2, EL2, ES2, ER2, EL2]),
    TileInfo::new(tile!(151, a), 1, [CJS0L2, CJL1R2, CN, CJS0R1, CN, CN], [ES3, ES3, EN, ES3, EN, EN]),
    TileInfo::new(tile!(151, b), 1, [CJS0L2, CJL1R2, CN, CJS0R1, CN, CN], [ES2, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(152, a), 1, [CJS0R2, CN, CN, CJS0L1, CN, CJR1L2], [ES3, EN, EN, ES3, EN, ES3]),
    TileInfo::new(tile!(152, b), 1, [CJS0R2, CN, CN, CJS0L1, CN, CJR1L2], [ES2, EN, EN, ES2, EN, ES2]),
    TileInfo::new(tile!(153, a), 1, [CS0, CS0, CN, CS0, CS0, CN], [ES3, ES2, EN, ES3, ES2, EN]),
    TileInfo::new(tile!(153, b), 1, [CS0, CN, CS0, CS0, CN, CS0], [ES2, EN, ES2, ES2, EN, ES2]),
    TileInfo::new(tile!(154, a), 1, [CN, CS0, CN, CL1, CS0, CR1], [EN, ES2, EN, ES3, ES2, ES3]),
    TileInfo::new(tile!(154, b), 1, [CN, CL1, CS0, CR1, CN, CS0], [EN, ES2, ES2, ES2, EN, ES2]),
    TileInfo::new(tile!(901, a), 1, [CN; 6], [EN; 6]),
    TileInfo::new(tile!(901, b), 1, [CN; 6], [EN; 6]),
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

        assert_eq!(TileId::new(101, 0, 0).base().to_string(), "101");
        assert_eq!(TileId::new(101, 1, 0).base().to_string(), "101");
        assert_eq!(TileId::new(102, 1, 0).base().to_string(), "102a");
        assert_eq!(TileId::new(102, 2, 0).base().to_string(), "102b");
        assert_eq!(TileId::new(104, 1, 2).base().to_string(), "104a");
    }

    #[test]
    fn id_from_str() {
        assert_eq!("0".parse::<TileId>(), Ok(TileId::new(0, 0, 0)));
        assert_eq!("101".parse::<TileId>(), Ok(TileId::new(101, 0, 0)));
        assert_eq!("102a".parse::<TileId>(), Ok(TileId::new(102, 1, 0)));
        assert_eq!("102b".parse::<TileId>(), Ok(TileId::new(102, 2, 0)));
        assert_eq!("103a-1".parse::<TileId>(), Ok(TileId::new(103, 1, 1)));
        assert_eq!("103b-3".parse::<TileId>(), Ok(TileId::new(103, 2, 3)));

        assert!("".parse::<TileId>().is_err());
        assert!("101x".parse::<TileId>().is_err());
    }

    #[test]
    fn id_serde() {
        let tile = tile!(102, a);
        let text = serde_json::to_string(&tile).unwrap();
        assert_eq!(text, r#""102a""#);

        let text = r#""103b-1""#;
        let tile: TileId = serde_json::from_str(&text).unwrap();
        assert_eq!(tile, TileId::new(103, 2, 1));

        let text = r#""#;
        let result: Result<TileId, _> = serde_json::from_str(&text);
        assert!(result.is_err());

        let text = r#""a-1""#;
        let result: Result<TileId, _> = serde_json::from_str(&text);
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
        let mut tiles = map.tiles().iter();
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(102, b, 0), ( 0,  0).into(), Direction::D)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(104, b, 1), (-1,  0).into(), Direction::A)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(113, b, 0), (-2,  0).into(), Direction::D)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(117, b, 1), (-2, -1).into(), Direction::E)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(114, b, 0), (-1, -2).into(), Direction::F)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(115, b, 1), (-1, -1).into(), Direction::D)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(115, b, 2), ( 0, -1).into(), Direction::C)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(108, b, 0), ( 1, -2).into(), Direction::F)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(110, b, 0), ( 1, -1).into(), Direction::B)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(107, b, 1), ( 1,  0).into(), Direction::B)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(101),       ( 0, -2).into(), Direction::A)));
        assert_eq!(tiles.next(), None);
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
        let mut tiles = map.tiles().iter();
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(102, b, 0), ( 1, -1).into(), Direction::E)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(104, b, 1), ( 1,  0).into(), Direction::B)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(113, b, 0), ( 1,  1).into(), Direction::E)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(117, b, 1), ( 0,  2).into(), Direction::F)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(114, b, 0), (-1,  2).into(), Direction::A)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(115, b, 1), ( 0,  1).into(), Direction::E)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(115, b, 2), ( 0,  0).into(), Direction::D)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(108, b, 0), (-1,  0).into(), Direction::A)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(110, b, 0), ( 0, -1).into(), Direction::C)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(107, b, 1), ( 1, -2).into(), Direction::C)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(101, a, 0), (-1,  1).into(), Direction::E)));
        assert_eq!(tiles.next(), None);
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
        let mut tiles = map.tiles().iter();
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(102, b, 0), ( 0, -1).into(), Direction::D)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(104, b, 1), ( 1, -1).into(), Direction::A)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(113, b, 0), ( 2, -1).into(), Direction::D)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(117, b, 1), ( 2,  0).into(), Direction::E)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(114, b, 0), ( 1,  1).into(), Direction::F)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(115, b, 1), ( 1,  0).into(), Direction::D)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(115, b, 2), ( 0,  0).into(), Direction::C)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(108, b, 0), (-1,  1).into(), Direction::F)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(110, b, 0), (-1,  0).into(), Direction::B)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(107, b, 1), (-1, -1).into(), Direction::B)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(101, a, 0), ( 0,  1).into(), Direction::D)));
        assert_eq!(tiles.next(), None);
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
        let mut tiles = map.tiles().iter();
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(102, b, 0), ( 1,  0).into(), Direction::F)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(104, b, 1), ( 0,  1).into(), Direction::C)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(113, b, 0), (-1,  2).into(), Direction::F)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(117, b, 1), (-2,  2).into(), Direction::A)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(114, b, 0), (-2,  1).into(), Direction::B)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(115, b, 1), (-1,  1).into(), Direction::F)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(115, b, 2), ( 0,  0).into(), Direction::E)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(108, b, 0), ( 0, -1).into(), Direction::B)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(110, b, 0), ( 1, -1).into(), Direction::D)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(107, b, 1), ( 2, -1).into(), Direction::D)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(101, a, 0), (-1,  0).into(), Direction::F)));
        assert_eq!(tiles.next(), None);
    }

    #[test]
    fn map_serde() {
        let map = Map::new();
        let text = serde_json::to_string(&map).unwrap();
        assert_eq!(text, r#"{"title":"My Track","tiles":[],"active_pos":{"q":0,"r":0},"active_dir":3}"#);

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
        let text = serde_json::to_string(&map).unwrap();
        assert_eq!(text, concat!(r#"{"title":"Short Track 2","tiles":["#,
            r#"{"id":"102b","q":1,"r":-1,"dir":4},"#,
            r#"{"id":"104b-1","q":1,"r":0,"dir":1},"#,
            r#"{"id":"113b","q":1,"r":1,"dir":4},"#,
            r#"{"id":"117b-1","q":0,"r":2,"dir":5},"#,
            r#"{"id":"114b","q":-1,"r":2,"dir":0},"#,
            r#"{"id":"115b-1","q":0,"r":1,"dir":4},"#,
            r#"{"id":"115b-2","q":0,"r":0,"dir":3},"#,
            r#"{"id":"108b","q":-1,"r":0,"dir":0},"#,
            r#"{"id":"110b","q":0,"r":-1,"dir":2},"#,
            r#"{"id":"107b-1","q":1,"r":-2,"dir":2},"#,
            r#"{"id":"101a","q":-1,"r":1,"dir":4}],"#,
            r#""active_pos":{"q":-1,"r":1},"active_dir":3}"#));

        let text = r#"{
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
        }"#;
        let map: Map = serde_json::from_str(&text).unwrap();
        assert_eq!(map.title(), "Short Track 2");
        let mut tiles = map.tiles().iter();
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(102, b, 0), ( 1, -1).into(), Direction::E)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(104, b, 1), ( 1,  0).into(), Direction::B)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(113, b, 0), ( 1,  1).into(), Direction::E)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(117, b, 1), ( 0,  2).into(), Direction::F)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(114, b, 0), (-1,  2).into(), Direction::A)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(115, b, 1), ( 0,  1).into(), Direction::E)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(115, b, 2), ( 0,  0).into(), Direction::D)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(108, b, 0), (-1,  0).into(), Direction::A)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(110, b, 0), ( 0, -1).into(), Direction::C)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(107, b, 1), ( 1, -2).into(), Direction::C)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(tile!(101, a, 0), (-1,  1).into(), Direction::E)));
        assert_eq!(tiles.next(), None);
        assert_eq!(map.active_pos(), None);
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

        let conn = Connection::JunctionLeft(0, 1);
        assert_eq!(conn == ConnectionHint::Straight, false);
        assert_eq!(conn == ConnectionHint::Left, true);
        assert_eq!(conn == ConnectionHint::Right, false);

        let conn = Connection::JunctionLeft(1, -2);
        assert_eq!(conn == ConnectionHint::Straight, false);
        assert_eq!(conn == ConnectionHint::Left, true);
        assert_eq!(conn == ConnectionHint::Right, false);

        let conn = Connection::JunctionRight(0, 2);
        assert_eq!(conn == ConnectionHint::Straight, false);
        assert_eq!(conn == ConnectionHint::Left, false);
        assert_eq!(conn == ConnectionHint::Right, true);

        let conn = Connection::JunctionRight(1, -2);
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

        let conn = Connection::JunctionLeft(0, 1);
        assert_eq!(conn.target(Direction::A), Some(Direction::D));
        assert_eq!(conn.target(Direction::D), Some(Direction::A));
        assert_eq!(conn.target(Direction::F), Some(Direction::C));

        let conn = Connection::JunctionLeft(1, -2);
        assert_eq!(conn.target(Direction::A), Some(Direction::C));
        assert_eq!(conn.target(Direction::D), Some(Direction::F));
        assert_eq!(conn.target(Direction::F), Some(Direction::B));

        let conn = Connection::JunctionRight(0, 2);
        assert_eq!(conn.target(Direction::A), Some(Direction::D));
        assert_eq!(conn.target(Direction::D), Some(Direction::A));
        assert_eq!(conn.target(Direction::F), Some(Direction::C));

        let conn = Connection::JunctionRight(1, -2);
        assert_eq!(conn.target(Direction::A), Some(Direction::E));
        assert_eq!(conn.target(Direction::D), Some(Direction::B));
        assert_eq!(conn.target(Direction::F), Some(Direction::D));
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
        assert_eq!(TILE_INFOS.iter().map(|info| info.count as u32).sum::<u32>(), 145);
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
    fn tile_info_connection_edge() {
        for tile in TILE_INFOS.iter() {
            for dir in Direction::iter() {
                let has_connection = match tile.conn[*dir] {
                    Connection::None => false,
                    _ => true,
                };
                let has_edge = match tile.edges[*dir] {
                    Edge::None => false,
                    _ => true,
                };
                assert_eq!(has_connection, has_edge,
                    "tile {}, direction {}: connection does not match according edge", tile, dir);

                let lanes = tile.edges[*dir].lanes();
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

        let text = r#"{"id":"0","q":0,"r":0,"dir":0}"#;
        let tile: PlacedTile = serde_json::from_str(&text).unwrap();
        assert_eq!(tile, PlacedTile::new(tile!(0), (0, 0).into(), Direction::A));

        let text = r#"{"id":"102a","q":0,"r":1,"dir":0}"#;
        let tile: PlacedTile = serde_json::from_str(&text).unwrap();
        assert_eq!(tile, PlacedTile::new(tile!(102, a), (0, 1).into(), Direction::A));

        let text = r#"{"id": "103b-1", "q": 1, "r": -2, "dir": 1}"#;
        let tile: PlacedTile = serde_json::from_str(&text).unwrap();
        assert_eq!(tile, PlacedTile::new(tile!(103, b, 1), (1, -2).into(), Direction::B));

        let text = r#"{}"#;
        let result: Result<PlacedTile, _> = serde_json::from_str(&text);
        assert!(result.is_err());

        let text = r#"{"id":"102a","q":0,"r":1}}"#;
        let result: Result<TileId, _> = serde_json::from_str(&text);
        assert!(result.is_err());
    }
}

//----------------------------------------------------------------------------
