//----------------------------------------------------------------------------
//! Tile identifier and characteristics.
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

/// Tile connection hint. Used to describe the orientation of adjacent tiles
/// along the track in [`Map::append()`](crate::map::Map::append).
///
/// Represents the track characteristics of a single tile's edge-to-edge
/// connection, simplified to one of three possible values: Straight, Left, or
/// Right.
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

/// Internal tile connection information. Describes how a specific tile edge is
/// connected to other edges of the same tile.
///
/// This is basically a more detailed version of [`ConnectionHint`] information,
/// used by [`Map`](crate::map::Map) to determine the best tile direction and
/// next active position when adding a tile.
///
/// Examples:
///
/// ```
/// # use rekee::tile::{Connection, ConnectionHint};
/// let conn = Connection::Left(1);
/// assert!(conn == ConnectionHint::Left);
/// assert!(conn != ConnectionHint::Right);
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Connection {
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

/// Information about tile characteristics like graphical variants, connections,
/// and edge types.
///
/// Use [`TileInfo::get()`] to lookup tile information by identifier.
///
/// Note that the list of tiles is static, it is not possible to add or change
/// any tile information entry at runtime.
///
/// Examples:
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
    conn: [Connection; 6],
    edges: [Edge; 6],
}

impl TileInfo {
    const fn new(id: TileId, count: usize, conn: [Connection; 6], edges: [Edge; 6]) -> Self {
        TileInfo { id, count, conn, edges }
    }

    /// Base identifier of the corresponding game tile.
    ///
    /// See [`TileId::base()`] for more information on the difference between
    /// base and full tile identifiers.
    ///
    /// Examples:
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
    /// Examples:
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
    /// Examples:
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

    pub fn connection(&self, dir: Direction) -> Connection {
        self.conn[dir]
    }

    pub fn connection_target(&self, source: Direction) -> Option<Direction> {
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
const CS3: Connection = Connection::Straight(3);
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

const TILE_INFOS: [TileInfo; 221] = [
    // Rallyman GT core box
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
    // Rallyman GT expansions
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
    // Rallyman DIRT core box
    TileInfo::new(tile!(201, a), 1, [CN, CN, CN, CS3, CN, CN], [EN, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(201, b), 1, [CN, CN, CN, CS3, CN, CN], [EN, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(202, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(202, b), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(203, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(203, b), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(204, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(204, b), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(205, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(205, b), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(206, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(206, b), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(207, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(207, b), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(208, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(208, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(209, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(209, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(210, a), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(210, b), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(211, a), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(211, b), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(212, a), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(212, b), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(213, a), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(213, b), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(214, a), 1, [CR0, CN, CN, CL0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(214, b), 1, [CR0, CN, CN, CL0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(215, a), 1, [CL0, CN, CN, CL0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(215, b), 1, [CL0, CN, CN, CL0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(216, a), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(216, b), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(217, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(217, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(218, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(218, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(219, a), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(219, b), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(220, a), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(220, b), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(221, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(221, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(222, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(222, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(223, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(223, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(224, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(224, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(225, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(225, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(226, a), 1, [CR0, CN, CN, CL0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(226, b), 1, [CR0, CN, CN, CL0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(227, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(227, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(228, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(228, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(229, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(229, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(230, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(230, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(231, a), 1, [CR0, CN, CN, CR0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(231, b), 1, [CR0, CN, CN, CR0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(232, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(232, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    // Rallyman DIRT 110% expansion
    TileInfo::new(tile!(301, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(301, b), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(302, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(302, b), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(303, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(303, b), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(304, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(304, b), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(305, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(305, b), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(306, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(306, b), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(307, a), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(307, b), 1, [CS0, CN, CN, CS0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(308, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(308, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(309, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(309, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(310, a), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(310, b), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(311, a), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(311, b), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(312, a), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(312, b), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(313, a), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(313, b), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(314, a), 1, [CL0, CN, CN, CL0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(314, b), 1, [CL0, CN, CN, CL0, CN, CN], [ES2, EN, EN, ES2, EN, EN]),
    TileInfo::new(tile!(315, a), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(315, b), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(316, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(316, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(317, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(317, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(318, a), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(318, b), 1, [CN, CN, CL2, CR2, CN, CN], [EN, EN, ES2, ES2, EN, EN]),
    TileInfo::new(tile!(319, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(319, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(320, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(320, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(321, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(321, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(322, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(322, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(323, a), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    TileInfo::new(tile!(323, b), 1, [CN, CL1, CN, CR1, CN, CN], [EN, ES2, EN, ES2, EN, EN]),
    // Unnumbered fillers
    TileInfo::new(tile!(901, a), 1, [CN; 6], [EN; 6]),
    TileInfo::new(tile!(901, b), 1, [CN; 6], [EN; 6]),
    TileInfo::new(tile!(902, a), 1, [CN; 6], [EN; 6]),
    TileInfo::new(tile!(902, b), 1, [CN; 6], [EN; 6]),
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
        assert_eq!(TILE_INFOS.iter().map(|info| info.count as u32).sum::<u32>(), 257);
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
}

//----------------------------------------------------------------------------
