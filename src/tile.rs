//----------------------------------------------------------------------------
// File: tile.rs
// $Id$
//----------------------------------------------------------------------------

use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

use crate::hexagon::{Coordinate, Direction, Layout, Point};

//----------------------------------------------------------------------------

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TileId {
    num: u16,
    side: u8,
    var: u8,
}

impl TileId {
    pub const fn new(num: u16, side: u8, var: u8) -> Self {
        TileId { num, side, var }
    }

    pub fn num(&self) -> u16 {
        self.num
    }

    pub fn side(&self) -> u8 {
        self.side
    }

    pub fn var(&self) -> u8 {
        self.var
    }

    pub fn base(&self) -> Self {
        let side = match self.num {
            // pitstop tile is the same on front and back
            101 => 0,
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

macro_rules! tile {
    ($num:literal) => {
        crate::tile::TileId::new($num, 0, 0)
    };
    ($num:literal, a) => {
        crate::tile::TileId::new($num, 1, 0)
    };
    ($num:literal, b) => {
        crate::tile::TileId::new($num, 2, 0)
    };
    ($num:literal, a, $var:literal) => {
        crate::tile::TileId::new($num, 1, $var)
    };
    ($num:literal, b, $var:literal) => {
        crate::tile::TileId::new($num, 2, $var)
    };
}

//----------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq)]
pub struct PlacedTile {
    pub id: TileId,
    pub pos: Coordinate,
    pub dir: Direction,
}

#[derive(Debug, Clone)]
pub struct Map {
    tiles: Vec<PlacedTile>,
}

impl Map {
    pub fn new() -> Self {
        let tiles = Vec::new();
        Map { tiles }
    }

    pub fn tiles(&self) -> &[PlacedTile] {
        &self.tiles
    }

    pub fn insert(&mut self, id: TileId, pos: Coordinate, dir: Direction) {
        self.tiles.retain(|tile| tile.pos != pos);
        self.tiles.push(PlacedTile { id, pos, dir });
    }

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
    }

    pub fn rotate_left(&mut self) {
        for tile in self.tiles.iter_mut() {
            tile.pos = tile.pos.rotated_left();
            tile.dir = tile.dir.rotated_left();
        }
    }

    pub fn rotate_right(&mut self) {
        for tile in self.tiles.iter_mut() {
            tile.pos = tile.pos.rotated_right();
            tile.dir = tile.dir.rotated_right();
        }
    }
}

//----------------------------------------------------------------------------

#[derive(Debug, Clone, Copy)]
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

//----------------------------------------------------------------------------

pub struct TileInfo {
    id: TileId,
    count: u8,
    conn: [Connection; 6],
}

impl TileInfo {
    const fn new(id: TileId, count: u8, conn: [Connection; 6]) -> Self {
        TileInfo { id, count, conn }
    }

    fn get(id: TileId) -> Option<&'static Self> {
        let idx = TILE_INFOS.binary_search_by_key(&id, |info| info.id);
        match idx {
            Ok(idx) => Some(&TILE_INFOS[idx]),
            Err(_) => None
        }
    }
}

impl fmt::Display for TileInfo {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "{}", self.id)
    }
}

//----------------------------------------------------------------------------

const N: Connection = Connection::None;
const S0: Connection = Connection::Straight(0);
const S1P: Connection = Connection::Straight(1);
const S1M: Connection = Connection::Straight(-1);
const L0: Connection = Connection::Left(0);
const L1: Connection = Connection::Left(1);
const L2: Connection = Connection::Left(2);
const R0: Connection = Connection::Right(0);
const R1: Connection = Connection::Right(1);
const R2: Connection = Connection::Right(2);
const JS0L1: Connection = Connection::JunctionLeft(0, 1);
const JS0L2: Connection = Connection::JunctionLeft(0, 2);
const JS0R1: Connection = Connection::JunctionRight(0, 1);
const JS0R2: Connection = Connection::JunctionRight(0, 2);
const JL1R2: Connection = Connection::JunctionLeft(1, -2);
const JR1L2: Connection = Connection::JunctionRight(1, -2);

const TILE_INFOS: [TileInfo; 109] = [
    TileInfo::new(tile!{101},    1, [N, N, N, N, N, N]),
    TileInfo::new(tile!{102, a}, 1, [S0, N, N, S0, N, N]),
    TileInfo::new(tile!{102, b}, 1, [S0, N, N, S0, N, N]),
    TileInfo::new(tile!{103, a}, 3, [S0, N, N, S0, N, N]),
    TileInfo::new(tile!{103, b}, 3, [S0, N, N, S0, N, N]),
    TileInfo::new(tile!{104, a}, 3, [S0, N, N, S0, N, N]),
    TileInfo::new(tile!{104, b}, 3, [S0, N, N, S0, N, N]),
    TileInfo::new(tile!{105, a}, 2, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{105, b}, 2, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{106, a}, 2, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{106, b}, 2, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{107, a}, 2, [N, N, R2, L2, N, N]),
    TileInfo::new(tile!{107, b}, 2, [N, N, R2, L2, N, N]),
    TileInfo::new(tile!{108, a}, 1, [N, N, R2, L2, N, N]),
    TileInfo::new(tile!{108, b}, 1, [N, N, R2, L2, N, N]),
    TileInfo::new(tile!{109, a}, 1, [N, N, R2, L2, N, N]),
    TileInfo::new(tile!{109, b}, 1, [N, N, R2, L2, N, N]),
    TileInfo::new(tile!{110, a}, 1, [R0, N, N, L0, N, N]),
    TileInfo::new(tile!{110, b}, 1, [R0, N, N, L0, N, N]),
    TileInfo::new(tile!{111, a}, 2, [N, N, R2, L2, N, N]),
    TileInfo::new(tile!{111, b}, 2, [N, N, R2, L2, N, N]),
    TileInfo::new(tile!{112, a}, 2, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{112, b}, 2, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{113, a}, 1, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{113, b}, 1, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{114, a}, 1, [N, N, R2, L2, N, N]),
    TileInfo::new(tile!{114, b}, 1, [N, N, R2, L2, N, N]),
    TileInfo::new(tile!{115, a}, 2, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{115, b}, 2, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{116, a}, 2, [R0, N, N, L0, N, N]),
    TileInfo::new(tile!{116, b}, 2, [R0, N, N, L0, N, N]),
    TileInfo::new(tile!{117, a}, 2, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{117, b}, 2, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{118, a}, 1, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{118, b}, 1, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{119, a}, 1, [S0, N, N, S0, N, N]),
    TileInfo::new(tile!{119, b}, 1, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{120, a}, 1, [S0, N, N, S0, N, N]),
    TileInfo::new(tile!{120, b}, 1, [N, N, N, L1, N, R1]),
    TileInfo::new(tile!{121, a}, 2, [N, N, S1M, N, S1P, N]),
    TileInfo::new(tile!{121, b}, 2, [N, N, S1M, N, S1P, N]),
    TileInfo::new(tile!{122, a}, 2, [N, N, N, N, L2, R2]),
    TileInfo::new(tile!{122, b}, 2, [N, N, N, N, L2, R2]),
    TileInfo::new(tile!{123, a}, 1, [N, L2, R2, N, N, N]),
    TileInfo::new(tile!{123, b}, 1, [N, L2, R2, N, N, N]),
    TileInfo::new(tile!{124, a}, 2, [L1, N, R1, N, N, N]),
    TileInfo::new(tile!{124, b}, 2, [L1, N, R1, N, N, N]),
    TileInfo::new(tile!{125, a}, 2, [R1, N, N, N, L1, N]),
    TileInfo::new(tile!{125, b}, 2, [R1, N, N, N, L1, N]),
    TileInfo::new(tile!{126, a}, 1, [N, N, N, L2, R2, N]),
    TileInfo::new(tile!{126, b}, 1, [N, N, N, L2, R2, N]),
    TileInfo::new(tile!{127, a}, 2, [N, N, L2, R2, N, N]),
    TileInfo::new(tile!{127, b}, 2, [N, N, L2, R2, N, N]),
    TileInfo::new(tile!{128, a}, 1, [N, N, N, N, L2, R2]),
    TileInfo::new(tile!{128, b}, 1, [N, N, N, N, L2, R2]),
    TileInfo::new(tile!{129, a}, 1, [N, R0, N, N, L0, N]),
    TileInfo::new(tile!{129, b}, 1, [N, R0, N, N, L0, N]),
    TileInfo::new(tile!{130, a}, 1, [N, N, R0, N, N, L0]),
    TileInfo::new(tile!{130, b}, 1, [N, N, R0, N, N, L0]),
    TileInfo::new(tile!{131, a}, 1, [N, N, S1M, N, S1P, N]),
    TileInfo::new(tile!{131, b}, 1, [N, N, S1M, N, S1P, N]),
    TileInfo::new(tile!{132, a}, 1, [N, N, S1M, N, S1P, N]),
    TileInfo::new(tile!{132, b}, 1, [N, N, S1M, N, S1P, N]),
    TileInfo::new(tile!{133, a}, 1, [N, S1P, S1M, N, S1P, S1M]),
    TileInfo::new(tile!{133, b}, 1, [N, S1P, S1M, N, S1P, S1M]),
    TileInfo::new(tile!{134, a}, 1, [N, N, R0, N, N, R0]),
    TileInfo::new(tile!{134, b}, 1, [N, N, R0, N, N, R0]),
    TileInfo::new(tile!{135, a}, 1, [N, L0, N, N, L0, N]),
    TileInfo::new(tile!{135, b}, 1, [N, L0, N, N, L0, N]),
    TileInfo::new(tile!{136, a}, 1, [N, N, L2, R2, N, N]),
    TileInfo::new(tile!{136, b}, 1, [N, N, L2, R2, N, N]),
    TileInfo::new(tile!{137, a}, 1, [N, N, L1, N, R1, N]),
    TileInfo::new(tile!{137, b}, 1, [N, N, L1, N, R1, N]),
    TileInfo::new(tile!{138, a}, 2, [N, N, L1, N, R1, N]),
    TileInfo::new(tile!{138, b}, 2, [N, N, L1, N, R1, N]),
    TileInfo::new(tile!{139, a}, 1, [S0, N, N, S0, N, N]),
    TileInfo::new(tile!{139, b}, 1, [N, S1P, S1M, N, S1P, S1M]),
    TileInfo::new(tile!{140, a}, 1, [S0, N, N, S0, N, N]),
    TileInfo::new(tile!{140, b}, 1, [N, S1P, S1M, N, S1P, S1M]),
    TileInfo::new(tile!{141, a}, 1, [S0, N, N, S0, N, N]),
    TileInfo::new(tile!{141, b}, 1, [S0, N, S0, S0, N, S0]),
    TileInfo::new(tile!{142, a}, 1, [S0, N, N, S0, N, N]),
    TileInfo::new(tile!{142, b}, 1, [S0, N, S1M, S0, S1P, N]),
    TileInfo::new(tile!{143, a}, 1, [S0, N, N, S0, N, N]),
    TileInfo::new(tile!{143, b}, 1, [N, S1P, S1M, N, S1P, S1M]),
    TileInfo::new(tile!{144, a}, 1, [S0, N, N, S0, N, N]),
    TileInfo::new(tile!{144, b}, 1, [N, S1P, S1M, N, S1P, S1M]),
    TileInfo::new(tile!{145, a}, 1, [S0, N, N, S0, N, N]),
    TileInfo::new(tile!{145, b}, 1, [S0, N, S0, S0, N, S0]),
    TileInfo::new(tile!{146, a}, 1, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{146, b}, 1, [S0, N, S1M, S0, S1P, N]),
    TileInfo::new(tile!{147, a}, 1, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{147, b}, 1, [N, S1P, N, L2, R2, S1M]),
    TileInfo::new(tile!{148, a}, 1, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{148, b}, 1, [N, S1P, L2, R2, N, S1M]),
    TileInfo::new(tile!{149, a}, 1, [N, L1, N, R1, N, N]),
    TileInfo::new(tile!{149, b}, 1, [N, S1P, L2, R2, N, S1M]),
    TileInfo::new(tile!{150, a}, 1, [S0, S1P, S1M, S0, S1P, S1M]),
    TileInfo::new(tile!{150, b}, 1, [S0, S1P, S1M, S0, S1P, S1M]),
    TileInfo::new(tile!{151, a}, 1, [JS0L2, JL1R2, N, JS0R1, N, N]),
    TileInfo::new(tile!{151, b}, 1, [JS0L2, JL1R2, N, JS0R1, N, N]),
    TileInfo::new(tile!{152, a}, 1, [JS0R2, N, N, JS0L1, N, JR1L2]),
    TileInfo::new(tile!{152, b}, 1, [JS0R2, N, N, JS0L1, N, JR1L2]),
    TileInfo::new(tile!{153, a}, 1, [S0, S0, N, S0, S0, N]),
    TileInfo::new(tile!{153, b}, 1, [S0, S0, N, S0, S0, N]),
    TileInfo::new(tile!{154, a}, 1, [N, S0, N, L1, S0, R1]),
    TileInfo::new(tile!{154, b}, 1, [N, L1, S0, R1, N, S0]),
    TileInfo::new(tile!{901, a}, 1, [N, N, N, N, N, N]),
    TileInfo::new(tile!{901, b}, 1, [N, N, N, N, N, N]),
];

//----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn id_to_str() {
        assert_eq!("0", TileId::default().to_string());
        assert_eq!("101", TileId::new(101, 0, 0).to_string());
        assert_eq!("102a", TileId::new(102, 1, 0).to_string());
        assert_eq!("102b", TileId::new(102, 2, 0).to_string());
        assert_eq!("103a-1", TileId::new(103, 1, 1).to_string());
        assert_eq!("103b-3", TileId::new(103, 2, 3).to_string());

        assert_eq!("101", TileId::new(101, 0, 0).base().to_string());
        assert_eq!("101", TileId::new(101, 1, 0).base().to_string());
        assert_eq!("102a", TileId::new(102, 1, 0).base().to_string());
        assert_eq!("102b", TileId::new(102, 2, 0).base().to_string());
        assert_eq!("104a", TileId::new(104, 1, 2).base().to_string());
    }

    #[test]
    fn id_from_str() {
        assert_eq!(Ok(TileId::new(0, 0, 0)), "0".parse::<TileId>());
        assert_eq!(Ok(TileId::new(101, 0, 0)), "101".parse::<TileId>());
        assert_eq!(Ok(TileId::new(102, 1, 0)), "102a".parse::<TileId>());
        assert_eq!(Ok(TileId::new(102, 2, 0)), "102b".parse::<TileId>());
        assert_eq!(Ok(TileId::new(103, 1, 1)), "103a-1".parse::<TileId>());
        assert_eq!(Ok(TileId::new(103, 2, 3)), "103b-3".parse::<TileId>());

        assert!("".parse::<TileId>().is_err());
        assert!("101x".parse::<TileId>().is_err());
    }

    #[test]
    fn map_align_center() {
        let mut map = Map::new();
        map.align_center();
        let mut tiles = map.tiles().iter();
        assert_eq!(None, tiles.next());

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
        assert_eq!(Some(&PlacedTile { id: tile!(102, b, 0), pos: ( 1, -1).into(), dir: Direction::E }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(104, b, 1), pos: ( 1,  0).into(), dir: Direction::B }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(113, b, 0), pos: ( 1,  1).into(), dir: Direction::E }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(117, b, 1), pos: ( 0,  2).into(), dir: Direction::F }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(114, b, 0), pos: (-1,  2).into(), dir: Direction::A }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(115, b, 1), pos: ( 0,  1).into(), dir: Direction::E }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(115, b, 2), pos: ( 0,  0).into(), dir: Direction::D }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(108, b, 0), pos: (-1,  0).into(), dir: Direction::A }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(110, b, 0), pos: ( 0, -1).into(), dir: Direction::C }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(107, b, 1), pos: ( 1, -2).into(), dir: Direction::C }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(101, a, 0), pos: (-1,  1).into(), dir: Direction::E }), tiles.next());
        assert_eq!(None, tiles.next());
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
        assert_eq!(Some(&PlacedTile { id: tile!(102, b, 0), pos: ( 0, -1).into(), dir: Direction::D }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(104, b, 1), pos: ( 1, -1).into(), dir: Direction::A }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(113, b, 0), pos: ( 2, -1).into(), dir: Direction::D }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(117, b, 1), pos: ( 2,  0).into(), dir: Direction::E }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(114, b, 0), pos: ( 1,  1).into(), dir: Direction::F }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(115, b, 1), pos: ( 1,  0).into(), dir: Direction::D }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(115, b, 2), pos: ( 0,  0).into(), dir: Direction::C }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(108, b, 0), pos: (-1,  1).into(), dir: Direction::F }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(110, b, 0), pos: (-1,  0).into(), dir: Direction::B }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(107, b, 1), pos: (-1, -1).into(), dir: Direction::B }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(101, a, 0), pos: ( 0,  1).into(), dir: Direction::D }), tiles.next());
        assert_eq!(None, tiles.next());
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
        assert_eq!(Some(&PlacedTile { id: tile!(102, b, 0), pos: ( 1,  0).into(), dir: Direction::F }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(104, b, 1), pos: ( 0,  1).into(), dir: Direction::C }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(113, b, 0), pos: (-1,  2).into(), dir: Direction::F }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(117, b, 1), pos: (-2,  2).into(), dir: Direction::A }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(114, b, 0), pos: (-2,  1).into(), dir: Direction::B }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(115, b, 1), pos: (-1,  1).into(), dir: Direction::F }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(115, b, 2), pos: ( 0,  0).into(), dir: Direction::E }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(108, b, 0), pos: ( 0, -1).into(), dir: Direction::B }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(110, b, 0), pos: ( 1, -1).into(), dir: Direction::D }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(107, b, 1), pos: ( 2, -1).into(), dir: Direction::D }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: tile!(101, a, 0), pos: (-1,  0).into(), dir: Direction::F }), tiles.next());
        assert_eq!(None, tiles.next());
    }

    #[test]
    fn connection_target() {
        let conn = Connection::None;
        assert_eq!(None, conn.target(Direction::A));
        assert_eq!(None, conn.target(Direction::D));
        assert_eq!(None, conn.target(Direction::F));

        let conn = Connection::Straight(0);
        assert_eq!(Some(Direction::D), conn.target(Direction::A));
        assert_eq!(Some(Direction::A), conn.target(Direction::D));
        assert_eq!(Some(Direction::C), conn.target(Direction::F));

        let conn = Connection::Straight(1);
        assert_eq!(Some(Direction::E), conn.target(Direction::A));
        assert_eq!(Some(Direction::B), conn.target(Direction::D));
        assert_eq!(Some(Direction::D), conn.target(Direction::F));

        let conn = Connection::Straight(-1);
        assert_eq!(Some(Direction::C), conn.target(Direction::A));
        assert_eq!(Some(Direction::F), conn.target(Direction::D));
        assert_eq!(Some(Direction::B), conn.target(Direction::F));

        let conn = Connection::Left(0);
        assert_eq!(Some(Direction::D), conn.target(Direction::A));
        assert_eq!(Some(Direction::A), conn.target(Direction::D));
        assert_eq!(Some(Direction::C), conn.target(Direction::F));

        let conn = Connection::Left(1);
        assert_eq!(Some(Direction::C), conn.target(Direction::A));
        assert_eq!(Some(Direction::F), conn.target(Direction::D));
        assert_eq!(Some(Direction::B), conn.target(Direction::F));

        let conn = Connection::Left(2);
        assert_eq!(Some(Direction::B), conn.target(Direction::A));
        assert_eq!(Some(Direction::E), conn.target(Direction::D));
        assert_eq!(Some(Direction::A), conn.target(Direction::F));

        let conn = Connection::Right(0);
        assert_eq!(Some(Direction::D), conn.target(Direction::A));
        assert_eq!(Some(Direction::A), conn.target(Direction::D));
        assert_eq!(Some(Direction::C), conn.target(Direction::F));

        let conn = Connection::Right(1);
        assert_eq!(Some(Direction::E), conn.target(Direction::A));
        assert_eq!(Some(Direction::B), conn.target(Direction::D));
        assert_eq!(Some(Direction::D), conn.target(Direction::F));

        let conn = Connection::Right(2);
        assert_eq!(Some(Direction::F), conn.target(Direction::A));
        assert_eq!(Some(Direction::C), conn.target(Direction::D));
        assert_eq!(Some(Direction::E), conn.target(Direction::F));

        let conn = Connection::JunctionLeft(0, 1);
        assert_eq!(Some(Direction::D), conn.target(Direction::A));
        assert_eq!(Some(Direction::A), conn.target(Direction::D));
        assert_eq!(Some(Direction::C), conn.target(Direction::F));

        let conn = Connection::JunctionLeft(1, -2);
        assert_eq!(Some(Direction::C), conn.target(Direction::A));
        assert_eq!(Some(Direction::F), conn.target(Direction::D));
        assert_eq!(Some(Direction::B), conn.target(Direction::F));

        let conn = Connection::JunctionRight(0, 2);
        assert_eq!(Some(Direction::D), conn.target(Direction::A));
        assert_eq!(Some(Direction::A), conn.target(Direction::D));
        assert_eq!(Some(Direction::C), conn.target(Direction::F));

        let conn = Connection::JunctionRight(1, -2);
        assert_eq!(Some(Direction::E), conn.target(Direction::A));
        assert_eq!(Some(Direction::B), conn.target(Direction::D));
        assert_eq!(Some(Direction::D), conn.target(Direction::F));
    }

    #[test]
    fn tile_info_get() {
        assert!(TileInfo::get("101".parse().unwrap()).is_some());
        assert!(TileInfo::get("101b".parse().unwrap()).is_none());
        assert!(TileInfo::get("102a".parse().unwrap()).is_some());
        assert!(TileInfo::get("103a".parse().unwrap()).is_some());
    }

    #[test]
    fn tile_info_count() {
        assert_eq!(145, TILE_INFOS.iter().map(|info| info.count as u32).sum::<u32>());
    }

    #[test]
    fn tile_info_sorted() {
        assert!(TILE_INFOS.iter().all(|info| info.id == info.id.base()));
        assert!(TILE_INFOS.windows(2).all(|infos| infos[0].id <= infos[1].id));
        // FIXME: could be replaced with is_sorted() once it's no longer unstable
        //assert!(TILE_INFOS.is_sorted_by_key(|info| info.id));
    }
}

//----------------------------------------------------------------------------
