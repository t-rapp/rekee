//----------------------------------------------------------------------------
// File: tile.rs
// $Id$
//----------------------------------------------------------------------------

use std::collections::BTreeMap;
use std::fmt;
use std::num::ParseIntError;
use std::str::FromStr;

use crate::hexagon::{Coordinate, Direction};

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

#[derive(Debug, Clone)]
pub struct PlacedTile {
    pub id: TileId,
    pub dir: Direction,
}

pub type Map = BTreeMap<Coordinate, PlacedTile>;

//----------------------------------------------------------------------------

pub struct TileInfo {
    id: TileId,
}

impl TileInfo {
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

const TILE_INFOS: [TileInfo; 107] = [
    TileInfo { id: tile!{101, a} },
    TileInfo { id: tile!{102, a} }, TileInfo { id: tile!{102, b} },
    TileInfo { id: tile!{103, a} }, TileInfo { id: tile!{103, b} },
    TileInfo { id: tile!{104, a} }, TileInfo { id: tile!{104, b} },
    TileInfo { id: tile!{105, a} }, TileInfo { id: tile!{105, b} },
    TileInfo { id: tile!{106, a} }, TileInfo { id: tile!{106, b} },
    TileInfo { id: tile!{107, a} }, TileInfo { id: tile!{107, b} },
    TileInfo { id: tile!{108, a} }, TileInfo { id: tile!{108, b} },
    TileInfo { id: tile!{109, a} }, TileInfo { id: tile!{109, b} },
    TileInfo { id: tile!{110, a} }, TileInfo { id: tile!{110, b} },
    TileInfo { id: tile!{111, a} }, TileInfo { id: tile!{111, b} },
    TileInfo { id: tile!{112, a} }, TileInfo { id: tile!{112, b} },
    TileInfo { id: tile!{113, a} }, TileInfo { id: tile!{113, b} },
    TileInfo { id: tile!{114, a} }, TileInfo { id: tile!{114, b} },
    TileInfo { id: tile!{115, a} }, TileInfo { id: tile!{115, b} },
    TileInfo { id: tile!{116, a} }, TileInfo { id: tile!{116, b} },
    TileInfo { id: tile!{117, a} }, TileInfo { id: tile!{117, b} },
    TileInfo { id: tile!{118, a} }, TileInfo { id: tile!{118, b} },
    TileInfo { id: tile!{119, a} }, TileInfo { id: tile!{119, b} },
    TileInfo { id: tile!{120, a} }, TileInfo { id: tile!{120, b} },
    TileInfo { id: tile!{121, a} }, TileInfo { id: tile!{121, b} },
    TileInfo { id: tile!{122, a} }, TileInfo { id: tile!{122, b} },
    TileInfo { id: tile!{123, a} }, TileInfo { id: tile!{123, b} },
    TileInfo { id: tile!{124, a} }, TileInfo { id: tile!{124, b} },
    TileInfo { id: tile!{125, a} }, TileInfo { id: tile!{125, b} },
    TileInfo { id: tile!{126, a} }, TileInfo { id: tile!{126, b} },
    TileInfo { id: tile!{127, a} }, TileInfo { id: tile!{127, b} },
    TileInfo { id: tile!{128, a} }, TileInfo { id: tile!{128, b} },
    TileInfo { id: tile!{129, a} }, TileInfo { id: tile!{129, b} },
    TileInfo { id: tile!{130, a} }, TileInfo { id: tile!{130, b} },
    TileInfo { id: tile!{131, a} }, TileInfo { id: tile!{131, b} },
    TileInfo { id: tile!{132, a} }, TileInfo { id: tile!{132, b} },
    TileInfo { id: tile!{133, a} }, TileInfo { id: tile!{133, b} },
    TileInfo { id: tile!{134, a} }, TileInfo { id: tile!{134, b} },
    TileInfo { id: tile!{135, a} }, TileInfo { id: tile!{135, b} },
    TileInfo { id: tile!{136, a} }, TileInfo { id: tile!{136, b} },
    TileInfo { id: tile!{137, a} }, TileInfo { id: tile!{137, b} },
    TileInfo { id: tile!{138, a} }, TileInfo { id: tile!{138, b} },
    TileInfo { id: tile!{139, a} }, TileInfo { id: tile!{139, b} },
    TileInfo { id: tile!{140, a} }, TileInfo { id: tile!{140, b} },
    TileInfo { id: tile!{141, a} }, TileInfo { id: tile!{141, b} },
    TileInfo { id: tile!{142, a} }, TileInfo { id: tile!{142, b} },
    TileInfo { id: tile!{143, a} }, TileInfo { id: tile!{143, b} },
    TileInfo { id: tile!{144, a} }, TileInfo { id: tile!{144, b} },
    TileInfo { id: tile!{145, a} }, TileInfo { id: tile!{145, b} },
    TileInfo { id: tile!{146, a} }, TileInfo { id: tile!{146, b} },
    TileInfo { id: tile!{147, a} }, TileInfo { id: tile!{147, b} },
    TileInfo { id: tile!{148, a} }, TileInfo { id: tile!{148, b} },
    TileInfo { id: tile!{149, a} }, TileInfo { id: tile!{149, b} },
    TileInfo { id: tile!{150, a} }, TileInfo { id: tile!{150, b} },
    TileInfo { id: tile!{151, a} }, TileInfo { id: tile!{151, b} },
    TileInfo { id: tile!{152, a} }, TileInfo { id: tile!{152, b} },
    TileInfo { id: tile!{153, a} }, TileInfo { id: tile!{153, b} },
    TileInfo { id: tile!{154, a} }, TileInfo { id: tile!{154, b} },
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
    fn tile_info_get() {
        assert!(TileInfo::get("101a".parse().unwrap()).is_some());
        assert!(TileInfo::get("101b".parse().unwrap()).is_none());
        assert!(TileInfo::get("102a".parse().unwrap()).is_some());
        assert!(TileInfo::get("103a".parse().unwrap()).is_some());
    }
}

//----------------------------------------------------------------------------
