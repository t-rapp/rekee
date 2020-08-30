//----------------------------------------------------------------------------
// File: tile.rs
// $Id$
//----------------------------------------------------------------------------

use std::fmt;

pub struct TileInfo {
    pub id: &'static str,
    num: u16,
    side: u8,
    var: u8,
}

impl TileInfo {
    fn get(id: &str) -> Option<&Self> {
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

macro_rules! tile {
    ($num:literal, a) => {
        crate::tile::TileInfo {
            id: concat!($num, 'a'),
            num: $num, side: 1, var: 0
        }
    };
    ($num:literal, b) => {
        crate::tile::TileInfo {
            id: concat!($num, 'b'),
            num: $num, side: 2, var: 0
        }
    };
    ($num:literal, a, $var:literal) => {
        crate::tile::TileInfo {
            id: concat!($num, 'a', '-', $var),
            num: $num, side: 1, var: $var
        }
    };
    ($num:literal, b, $var:literal) => {
        crate::tile::TileInfo {
            id: concat!($nr, 'b', '-', $vr),
            num: $num, side: 2, var: $var
        }
    };
}

const TILE_INFOS: [TileInfo; 107] = [
    tile!{101, a},
    tile!{102, a}, tile!{102, b},
    tile!{103, a}, tile!{103, b},
    tile!{104, a}, tile!{104, b},
    tile!{105, a}, tile!{105, b},
    tile!{106, a}, tile!{106, b},
    tile!{107, a}, tile!{107, b},
    tile!{108, a}, tile!{108, b},
    tile!{109, a}, tile!{109, b},
    tile!{110, a}, tile!{110, b},
    tile!{111, a}, tile!{111, b},
    tile!{112, a}, tile!{112, b},
    tile!{113, a}, tile!{113, b},
    tile!{114, a}, tile!{114, b},
    tile!{115, a}, tile!{115, b},
    tile!{116, a}, tile!{116, b},
    tile!{117, a}, tile!{117, b},
    tile!{118, a}, tile!{118, b},
    tile!{119, a}, tile!{119, b},
    tile!{120, a}, tile!{120, b},
    tile!{121, a}, tile!{121, b},
    tile!{122, a}, tile!{122, b},
    tile!{123, a}, tile!{123, b},
    tile!{124, a}, tile!{124, b},
    tile!{125, a}, tile!{125, b},
    tile!{126, a}, tile!{126, b},
    tile!{127, a}, tile!{127, b},
    tile!{128, a}, tile!{128, b},
    tile!{129, a}, tile!{129, b},
    tile!{130, a}, tile!{130, b},
    tile!{131, a}, tile!{131, b},
    tile!{132, a}, tile!{132, b},
    tile!{133, a}, tile!{133, b},
    tile!{134, a}, tile!{134, b},
    tile!{135, a}, tile!{135, b},
    tile!{136, a}, tile!{136, b},
    tile!{137, a}, tile!{137, b},
    tile!{138, a}, tile!{138, b},
    tile!{139, a}, tile!{139, b},
    tile!{140, a}, tile!{140, b},
    tile!{141, a}, tile!{141, b},
    tile!{142, a}, tile!{142, b},
    tile!{143, a}, tile!{143, b},
    tile!{144, a}, tile!{144, b},
    tile!{145, a}, tile!{145, b},
    tile!{146, a}, tile!{146, b},
    tile!{147, a}, tile!{147, b},
    tile!{148, a}, tile!{148, b},
    tile!{149, a}, tile!{149, b},
    tile!{150, a}, tile!{150, b},
    tile!{151, a}, tile!{151, b},
    tile!{152, a}, tile!{152, b},
    tile!{153, a}, tile!{153, b},
    tile!{154, a}, tile!{154, b},
];

//----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tile_to_str() {
        assert_eq!("101a", tile!(101, a).id);
        assert_eq!("102b", tile!(102, b).id);
        assert_eq!("103a-1", tile!(103, a, 1).id);
    }

    #[test]
    fn tile_info_get() {
        assert!(TileInfo::get("101a").is_some());
        assert!(TileInfo::get("101b").is_none());
        assert!(TileInfo::get("102a").is_some());
        assert!(TileInfo::get("103a").is_some());
    }
}

//----------------------------------------------------------------------------
