//----------------------------------------------------------------------------
//! Import and export of map data.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use std::convert::{TryFrom, TryInto};
use std::fmt;

use serde::{Deserialize, Serialize};

use crate::edition::Series;
use crate::hexagon::{FloatCoordinate, FloatDirection};
use crate::map::{PlacedToken, Map};
use crate::tile::{Terrain, TileId, TileInfo};
use crate::token::TokenId;
use super::*;

//----------------------------------------------------------------------------

type Result<T> = std::result::Result<T, ImportError>;

#[derive(Debug)]
pub enum ImportError {
    UnknownTileId(String),
    UnknownTokenId(String),
    UnsupportedTokenId(String),
    JsonError(serde_json::Error),
}

impl fmt::Display for ImportError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ImportError::UnknownTileId(val) =>
                write!(fmt, "Unknown tile identifier \"{}\"", val),
            ImportError::UnknownTokenId(val) =>
                write!(fmt, "Unknown token identifier \"{}\"", val),
            ImportError::UnsupportedTokenId(val) =>
                write!(fmt, "Unsupported token identifier \"{}\"", val),
            ImportError::JsonError(err) =>
                write!(fmt, "Error processing JSON data: {}", err),
        }
    }
}

impl From<serde_json::Error> for ImportError {
    fn from(error: serde_json::Error) -> Self {
        ImportError::JsonError(error)
    }
}

//----------------------------------------------------------------------------

/// Import map data in native Rekee format.
pub fn import_native(data: &str) -> Result<Map> {
    native::import(data)
}

/// Export map data in native Rekee format.
pub fn export_native(map: &Map) -> Result<String> {
    native::export(map)
}

mod native {
    use super::*;

    #[derive(Debug, Default, Deserialize, Serialize)]
    struct ImportRoot {
        #[serde(default)]
        title: String,
        #[serde(default, skip_serializing_if = "String::is_empty")]
        author: String,
        tiles: Vec<ImportTile>,
    }

    #[derive(Debug, Default, Deserialize, Serialize)]
    struct ImportTile {
        q: i32,
        r: i32,
        id: String,
        dir: i32,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        tokens: Vec<ImportToken>,
    }

    #[derive(Debug, Default, Deserialize, Serialize)]
    struct ImportToken {
        id: String,
        q: f32,
        r: f32,
        dir: f32,
    }

    impl TryInto<PlacedToken> for &ImportToken {
        type Error = ImportError;

        fn try_into(self) -> Result<PlacedToken> {
            let id = self.id.parse::<TokenId>()
                .map_err(|_| ImportError::UnknownTokenId(self.id.clone()))?;
            let pos = FloatCoordinate::new(self.q, self.r);
            let dir = FloatDirection(self.dir);
            Ok(PlacedToken::new(id, pos, dir))
        }
    }

    impl From<&PlacedToken> for ImportToken {
        fn from(value: &PlacedToken) -> ImportToken {
            let id = format!("{:x}", value.id);
            let q = value.pos.q();
            let r = value.pos.r();
            let dir = f32::from(value.dir);
            ImportToken { id, q, r, dir }
        }
    }

    pub fn import(data: &str) -> Result<Map> {
        let data: ImportRoot = serde_json::from_str(data)?;

        let mut map = Map::new();
        if !data.title.is_empty() {
            map.set_title(&data.title);
        }
        if !data.author.is_empty() {
            map.set_author(&data.author);
        }
        for tile in data.tiles {
            let pos = Coordinate::new(tile.q, tile.r);
            let id = tile.id.parse::<TileId>()
                .map_err(|_| ImportError::UnknownTileId(tile.id.clone()));
            let dir = Direction::from(tile.dir);
            let tokens = tile.tokens.iter()
                // tokens that have no corresponding import identifier will be silently dropped here
                .filter_map(|token| token.try_into().ok())
                .collect();
            if let Ok(id) = id {
                map.insert_with_tokens(id, pos, dir, tokens);
            }
        }

        Ok(map)
    }

    pub fn export(map: &Map) -> Result<String> {
        let mut data = ImportRoot {
            title: map.title().to_string(),
            author: map.author().to_string(),
            tiles: Vec::with_capacity(map.tiles().len()),
        };

        for tile in map.tiles() {
            let q = tile.pos.q();
            let r = tile.pos.r();
            let id = format!("{:x}", tile.id());
            let dir = i32::from(tile.dir);
            let tokens = tile.tokens.iter()
                .map(|token| token.into())
                .collect();
            data.tiles.push(ImportTile { q, r, id, dir, tokens });
        }

        serde_json::to_string(&data)
            .map_err(ImportError::JsonError)
    }
}

//----------------------------------------------------------------------------

/// Import map data in RGT format.
///
/// Allows to read files generated by the Rallyman GT Track Editor software.
pub fn import_rgt(data: &str) -> Result<Map> {
    rgt::import(data)
}

/// Export map data in RGT format.
///
/// Allows to write files compatible with the Rallyman GT Track Editor software.
pub fn export_rgt(map: &Map) -> Result<String> {
    rgt::export(map)
}

mod rgt {
    use super::*;

    #[derive(Debug, Default, Deserialize, Serialize)]
    struct ImportRoot {
        #[serde(rename = "Path", default)]
        path: String,
        #[serde(rename = "Name", default)]
        title: String,
        #[serde(rename = "Author", default, skip_serializing_if = "String::is_empty")]
        author: String,
        #[serde(rename = "Tuiles")]
        tiles: Vec<ImportTile>,
        #[serde(rename = "lowResWidth", default)]
        low_res_width: u32,
        #[serde(rename = "lowResHeight", default)]
        low_res_height: u32,
    }

    #[derive(Debug, Default, Deserialize, Serialize)]
    struct ImportTile {
        #[serde(rename = "X")]
        x: i32,
        #[serde(rename = "Y")]
        y: i32,
        #[serde(rename = "Orientation")]
        dir: i32,
        #[serde(rename = "TuileId")]
        id: String,
        #[serde(rename = "Tokens", default, skip_serializing_if = "Vec::is_empty")]
        tokens: Vec<ImportToken>,
    }

    #[derive(Debug, Default, Deserialize, Serialize)]
    struct ImportToken {
        #[serde(rename = "X")]
        x: f32,
        #[serde(rename = "Y")]
        y: f32,
        #[serde(rename = "Orientation")]
        dir: f32,
        #[serde(rename = "TokenId")]
        id: ImportTokenId,
    }

    impl TryFrom<&ImportToken> for PlacedToken {
        type Error = ImportError;

        fn try_from(value: &ImportToken) -> Result<PlacedToken> {
            let id = (&value.id).try_into()?;
            let pos = FloatCoordinate::from_pixel(&token_layout(), Point(value.x, value.y));
            let dir = FloatDirection::from_angle(value.dir);
            Ok(PlacedToken::new(id, pos, dir))
        }
    }

    #[derive(Debug, Default, Deserialize, Serialize)]
    #[serde(transparent)]
    struct ImportTokenId(String);

    impl TryInto<TokenId> for &ImportTokenId {
        type Error = ImportError;

        fn try_into(self) -> Result<TokenId> {
            match self.0.as_ref() {
                "asphalt1" =>
                    Ok(TokenId::ChicaneWithLimit(Terrain::Asphalt)),
                "asphalt2" =>
                    Ok(TokenId::Chicane(Terrain::Asphalt)),
                "asphalt3" =>
                    Ok(TokenId::Water(Terrain::Asphalt)),
                "asphalt4" =>
                    Ok(TokenId::Jump(Terrain::Asphalt)),
                "dirt1" =>
                    Ok(TokenId::ChicaneWithLimit(Terrain::Gravel)),
                "dirt2" =>
                    Ok(TokenId::Chicane(Terrain::Gravel)),
                "dirt3" =>
                    Ok(TokenId::Water(Terrain::Gravel)),
                "dirt4" =>
                    Ok(TokenId::Jump(Terrain::Gravel)),
                "ice1" =>
                    Ok(TokenId::ChicaneWithLimit(Terrain::Snow)),
                "ice2" =>
                    Ok(TokenId::Chicane(Terrain::Snow)),
                "ice3" =>
                    Ok(TokenId::Water(Terrain::Snow)),
                "ice4" =>
                    Ok(TokenId::Jump(Terrain::Snow)),
                "climbm1" =>
                    Ok(TokenId::ClimbAscent),
                "climbp1" =>
                    Ok(TokenId::ClimbDescent),
                "climbw" =>
                    Ok(TokenId::Cloud),
                "climbr1" =>
                    Ok(TokenId::Oxygen(1)),
                "climbr2" =>
                    Ok(TokenId::Oxygen(2)),
                "climbr3" =>
                    Ok(TokenId::Oxygen(3)),
                "greendoor" =>
                    Ok(TokenId::JokerEntrance),
                "reddoor" =>
                    Ok(TokenId::JokerExit),
                "whitedoor" =>
                    Ok(TokenId::Finish),
                _ =>
                    Err(ImportError::UnknownTokenId(self.0.clone())),
            }
        }
    }

    impl TryFrom<TokenId> for ImportTokenId {
        type Error = ImportError;

        fn try_from(value: TokenId) -> Result<ImportTokenId> {
            match value {
                TokenId::ChicaneWithLimit(Terrain::Asphalt) =>
                    Ok(ImportTokenId("asphalt1".into())),
                TokenId::ChicaneWithLimit(Terrain::Gravel) =>
                    Ok(ImportTokenId("dirt1".into())),
                TokenId::ChicaneWithLimit(Terrain::Snow) =>
                    Ok(ImportTokenId("ice1".into())),
                TokenId::ChicaneWithLimit(_) =>
                    Ok(ImportTokenId("dirt1".into())),
                TokenId::Chicane(Terrain::Asphalt) =>
                    Ok(ImportTokenId("asphalt2".into())),
                TokenId::Chicane(Terrain::Gravel) =>
                    Ok(ImportTokenId("dirt2".into())),
                TokenId::Chicane(Terrain::Snow) =>
                    Ok(ImportTokenId("ice2".into())),
                TokenId::Chicane(_) =>
                    Ok(ImportTokenId("dirt2".into())),
                TokenId::Water(Terrain::Asphalt) =>
                    Ok(ImportTokenId("asphalt3".into())),
                TokenId::Water(Terrain::Gravel) =>
                    Ok(ImportTokenId("dirt3".into())),
                TokenId::Water(Terrain::Snow) =>
                    Ok(ImportTokenId("ice3".into())),
                TokenId::Water(_) =>
                    Ok(ImportTokenId("dirt3".into())),
                TokenId::Jump(Terrain::Asphalt) =>
                    Ok(ImportTokenId("asphalt4".into())),
                TokenId::Jump(Terrain::Gravel) =>
                    Ok(ImportTokenId("dirt4".into())),
                TokenId::Jump(Terrain::Snow) =>
                    Ok(ImportTokenId("ice4".into())),
                TokenId::Jump(_) =>
                    Ok(ImportTokenId("dirt4".into())),
                TokenId::ClimbAscent =>
                    Ok(ImportTokenId("climbm1".into())),
                TokenId::ClimbDescent =>
                    Ok(ImportTokenId("climbp1".into())),
                TokenId::Cloud =>
                    Ok(ImportTokenId("climbw".into())),
                TokenId::Oxygen(1) =>
                    Ok(ImportTokenId("climbr1".into())),
                TokenId::Oxygen(2) =>
                    Ok(ImportTokenId("climbr2".into())),
                TokenId::Oxygen(3) =>
                    Ok(ImportTokenId("climbr3".into())),
                TokenId::JokerEntrance =>
                    Ok(ImportTokenId("greendoor".into())),
                TokenId::JokerExit =>
                    Ok(ImportTokenId("reddoor".into())),
                TokenId::Finish =>
                    Ok(ImportTokenId("whitedoor".into())),
                _ =>
                    Err(ImportError::UnsupportedTokenId(value.to_string())),
            }
        }
    }

    const IMPORT_OFFSET: Coordinate = Coordinate::new(-81, 52);

    pub fn import(data: &str) -> Result<Map> {
        let data: ImportRoot = serde_json::from_str(data)?;

        let mut map = Map::new();
        if !data.title.is_empty() {
            map.set_title(&data.title);
        }
        if !data.author.is_empty() {
            map.set_author(&data.author);
        }
        for tile in data.tiles {
            // convert from offset to axial coordinates
            let q = -tile.x - ((tile.y & 1) + tile.y) / 2;
            let r = tile.y;
            let pos = Coordinate::new(q, r) - IMPORT_OFFSET;
            let id = tile.id.parse::<TileId>()
                // special mapping for pit-stop tile
                .map(|id| if id.num() == 101 { id.base() } else { id })
                .unwrap_or_default();
            let dir = Direction::from(tile.dir);
            let tokens = tile.tokens.iter()
                .filter_map(|token| token.try_into().ok())
                .collect();
            debug!("import tile ({}, {}), {}, {} -> {}, {}, {}",
                tile.x, tile.y, tile.id, tile.dir, pos, id, dir);
            map.insert_with_tokens(id, pos, dir, tokens);
        }

        Ok(map)
    }

    pub fn export(map: &Map) -> Result<String> {
        // generate some default values for data that is not used in our editor
        let mut data = ImportRoot {
            path: "C:\\Applications\\RallymanGT Track Editor\\My Tracks".to_string(),
            title: map.title().to_string(),
            author: map.author().to_string(),
            tiles: Vec::with_capacity(map.tiles().len()),
            low_res_width: 2480,
            low_res_height: 1748,
        };

        let token_layout = token_layout();
        for tile in map.tiles() {
            let pos = tile.pos + IMPORT_OFFSET;
            // convert from axial to offset coordinates
            let x = -pos.q() - ((pos.r() & 1) + pos.r()) / 2;
            let y = pos.r();
            let mut id = match tile.id().num() {
                // special mapping for pit-stop tile
                101 => "101a".to_string(),
                _ => tile.id().to_string(),
            };
            if let Some(info) = TileInfo::get(tile.id()) {
                if info.series() == Series::Dirt {
                    // DIRT tiles are handled uppercase
                    id.make_ascii_uppercase();
                }
            }
            let dir = i32::from(tile.dir);
            debug!("export tile {}, {}, {} -> ({}, {}), {}, {}",
                tile.pos, tile.id(), tile.dir, x, y, &id, dir);

            let mut tokens = Vec::with_capacity(tile.tokens.len());
            for token in &tile.tokens {
                let pos = token.pos.to_pixel(&token_layout);
                let x = pos.x();
                let y = pos.y();
                let dir = token.dir.to_angle();
                let id = token.id.try_into()?;
                tokens.push(ImportToken { x, y, dir, id });
            }

            data.tiles.push(ImportTile { x, y, dir, id, tokens });
        }

        serde_json::to_string(&data)
            .map_err(ImportError::JsonError)
    }

    fn token_layout() -> Layout {
        // invert the x-axis, keep the y-axis
        Layout::new(Orientation::pointy(), Point(-1.0, 1.0), Point(0.0, 0.0))
    }
}

//----------------------------------------------------------------------------

/// Import map data with format auto-detection.
pub fn import_auto(data: &str) -> Result<Map> {
    // try our native format first
    let result = native::import(data);
    if result.is_ok() {
        return result;
    }
    // then try other formats
    if let Ok(map) = rgt::import(data) {
        return Ok(map);
    }
    // no format succeeded, return the first error
    result
}

/// Helper function to generate a valid filename from a (map title) string.
pub fn build_file_name(name: &str) -> String {
    let mut result: String = name.trim().chars()
        // remove all control characters
        .filter(|chr| !chr.is_control())
        // replace path separators
        .map(|chr| if chr != '/' && chr != '\\' { chr } else { '_' })
        // remove special characters on Microsoft Windows
        .filter(|chr| *chr != '<' && *chr != '>' && *chr != ':' &&
                      *chr != '"' && *chr != '|' && *chr != '?' && *chr != '*')
        .collect();
    if result.is_empty() {
        result.push_str("MyTrack");
    }
    result
}

//----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use approx::assert_abs_diff_eq;
    use crate::map::PlacedTile;
    use super::*;

    #[test]
    fn import_native_chicane_example() {
        let data = include_str!("tests/chicane-example1.json");
        let map = import_native(data)
            .expect("Cannot parse import file data");
        assert_eq!(map.title(), "Chicane Example 01");
        assert_abs_diff_eq!(map.tiles(), &[
            PlacedTile::new(TileId::new(202, 1, 0), (1, 0).into(), Direction::A),
            PlacedTile::with_tokens(TileId::new(205, 1, 0), (0, 0).into(), Direction::A, vec![
                PlacedToken::new(TokenId::ChicaneWithLimit(Terrain::Gravel), (0.0, 0.0).into(), FloatDirection(3.0)),
                PlacedToken::new(TokenId::Chicane(Terrain::Gravel), (0.33, 0.0).into(), FloatDirection(0.0)),
            ]),
        ][..]);

        let data = include_str!("tests/chicane-example2.json");
        let map = import_native(data)
            .expect("Cannot parse import file data");
        assert_eq!(map.title(), "Chicane Example 02");
        assert_abs_diff_eq!(map.tiles(), &[
            PlacedTile::new(TileId::new(202, 1, 0), (1, -1).into(), Direction::F),
            PlacedTile::with_tokens(TileId::new(205, 1, 0), (0,  0).into(), Direction::F, vec![
                PlacedToken::new(TokenId::ChicaneWithLimit(Terrain::Gravel), (0.0, 0.0).into(), FloatDirection(3.0)),
                PlacedToken::new(TokenId::Chicane(Terrain::Gravel), (0.33, 0.0).into(), FloatDirection(0.0)),
            ]),
        ][..]);
    }

    #[test]
    fn export_native_chicane_example() {
        let mut map = Map::new();
        map.set_title("Chicane Example 01");
        map.insert(TileId::new(202, 1, 0), (1, 0).into(), Direction::A);
        map.insert_with_tokens(TileId::new(205, 1, 0), (0, 0).into(), Direction::A, vec![
            PlacedToken::new(TokenId::ChicaneWithLimit(Terrain::Gravel), (0.0, 0.0).into(), FloatDirection(3.0)),
            PlacedToken::new(TokenId::Chicane(Terrain::Gravel), (0.33, 0.0).into(), FloatDirection(0.0)),
        ]);
        let data = export_native(&map)
            .expect("Cannot export track data");
        assert_eq!(data, r#"{"title":"Chicane Example 01","tiles":[{"q":1,"r":0,"id":"202a","dir":0},{"q":0,"r":0,"id":"205a","dir":0,"tokens":[{"id":"chicane-limit-gravel","q":0.0,"r":0.0,"dir":3.0},{"id":"chicane-gravel","q":0.33,"r":0.0,"dir":0.0}]}]}"#);

        let mut map = Map::new();
        map.set_title("Chicane Example 02");
        map.insert(TileId::new(202, 1, 0), (1, -1).into(), Direction::F);
        map.insert_with_tokens(TileId::new(205, 1, 0), (0, 0).into(), Direction::F, vec![
            PlacedToken::new(TokenId::ChicaneWithLimit(Terrain::Gravel), (0.0, 0.0).into(), FloatDirection(3.0)),
            PlacedToken::new(TokenId::Chicane(Terrain::Gravel), (0.33, 0.0).into(), FloatDirection(0.0)),
        ]);
        let data = export_native(&map)
            .expect("Cannot export track data");
        assert_eq!(data, r#"{"title":"Chicane Example 02","tiles":[{"q":1,"r":-1,"id":"202a","dir":5},{"q":0,"r":0,"id":"205a","dir":5,"tokens":[{"id":"chicane-limit-gravel","q":0.0,"r":0.0,"dir":3.0},{"id":"chicane-gravel","q":0.33,"r":0.0,"dir":0.0}]}]}"#);
        }

    #[test]
    fn import_native_short_track() {
        let data = include_str!("tests/short-track2.json");
        let map = import_native(data)
            .expect("Cannot parse import file data");
        assert_eq!(map.title(), "ShortTrack2");
        assert_eq!(map.author(), "CarmLima");
        assert_abs_diff_eq!(map.tiles(), &[
            PlacedTile::new(TileId::new(102, 2, 0), ( 0,  1).into(), Direction::A),
            PlacedTile::new(TileId::new(106, 2, 2), ( 1,  1).into(), Direction::C),
            PlacedTile::new(TileId::new(116, 2, 2), ( 0,  0).into(), Direction::A),
            PlacedTile::new(TileId::new(117, 2, 2), ( 1,  0).into(), Direction::C),
            PlacedTile::new(TileId::new(111, 2, 2), ( 2, -1).into(), Direction::F),
            PlacedTile::new(TileId::new(125, 2, 1), (-1,  0).into(), Direction::A),
            PlacedTile::new(TileId::new(112, 2, 1), ( 2,  0).into(), Direction::B),
            PlacedTile::new(TileId::new(128, 2, 0), (-1, -1).into(), Direction::D),
            PlacedTile::new(TileId::new(138, 2, 1), (-2,  0).into(), Direction::D),
            PlacedTile::new(TileId::new(104, 2, 2), (-1,  1).into(), Direction::A),
            PlacedTile::new(TileId::new(105, 2, 1), (-2,  1).into(), Direction::D),
        ][..]);
    }

    #[test]
    fn export_native_short_track() {
        let mut map = Map::new();
        map.set_title("ShortTrack2");
        map.set_author("CarmLima");
        map.insert(TileId::new(102, 2, 0), ( 0,  1).into(), Direction::A);
        map.insert(TileId::new(106, 2, 2), ( 1,  1).into(), Direction::C);
        map.insert(TileId::new(116, 2, 2), ( 0,  0).into(), Direction::A);
        map.insert(TileId::new(117, 2, 2), ( 1,  0).into(), Direction::C);
        map.insert(TileId::new(111, 2, 2), ( 2, -1).into(), Direction::F);
        map.insert(TileId::new(125, 2, 1), (-1,  0).into(), Direction::A);
        map.insert(TileId::new(112, 2, 1), ( 2,  0).into(), Direction::B);
        map.insert(TileId::new(128, 2, 0), (-1, -1).into(), Direction::D);
        map.insert(TileId::new(138, 2, 1), (-2,  0).into(), Direction::D);
        map.insert(TileId::new(104, 2, 2), (-1,  1).into(), Direction::A);
        map.insert(TileId::new(105, 2, 1), (-2,  1).into(), Direction::D);
        let data = export_native(&map)
            .expect("Cannot export track data");
        assert_eq!(data, r#"{"title":"ShortTrack2","author":"CarmLima","tiles":[{"q":0,"r":1,"id":"102b","dir":0},{"q":1,"r":1,"id":"106b-2","dir":2},{"q":0,"r":0,"id":"116b-2","dir":0},{"q":1,"r":0,"id":"117b-2","dir":2},{"q":2,"r":-1,"id":"111b-2","dir":5},{"q":-1,"r":0,"id":"125b-1","dir":0},{"q":2,"r":0,"id":"112b-1","dir":1},{"q":-1,"r":-1,"id":"128b","dir":3},{"q":-2,"r":0,"id":"138b-1","dir":3},{"q":-1,"r":1,"id":"104b-2","dir":0},{"q":-2,"r":1,"id":"105b-1","dir":3}]}"#);
    }

    #[test]
    fn import_rgt_chicane_example() {
        let data = include_str!("tests/chicane-example1.rgt");
        let map = import_rgt(data)
            .expect("Cannot parse import file data");
        assert_eq!(map.title(), "Chicane Example 01");
        assert_eq!(map.author(), "Holy Grail Games");
        assert_abs_diff_eq!(map.tiles(), &[
            PlacedTile::new(TileId::new(202, 1, 0), (1, 0).into(), Direction::A),
            PlacedTile::with_tokens(TileId::new(205, 1, 0), (0, 0).into(), Direction::A, vec![
                PlacedToken::new(TokenId::ChicaneWithLimit(Terrain::Gravel), (0.0, 0.0).into(), FloatDirection(3.0)),
                PlacedToken::new(TokenId::Chicane(Terrain::Gravel), (0.33, 0.0).into(), FloatDirection(0.0)),
            ]),
        ][..]);

        let data = include_str!("tests/chicane-example2.rgt");
        let map = import_rgt(data)
            .expect("Cannot parse import file data");
        assert_eq!(map.title(), "Chicane Example 02");
        assert_eq!(map.author(), "Holy Grail Games");
        assert_abs_diff_eq!(map.tiles(), &[
            PlacedTile::new(TileId::new(202, 1, 0), (0, 0).into(), Direction::F),
            PlacedTile::with_tokens(TileId::new(205, 1, 0), (-1,  1).into(), Direction::F, vec![
                PlacedToken::new(TokenId::ChicaneWithLimit(Terrain::Gravel), (0.0, 0.0).into(), FloatDirection(3.0)),
                PlacedToken::new(TokenId::Chicane(Terrain::Gravel), (0.33, 0.0).into(), FloatDirection(0.0)),
            ]),
        ][..]);
    }

    #[test]
    fn export_rgt_chicane_example() {
        let mut map = Map::new();
        map.set_title("Chicane Example 01");
        map.set_author("Holy Grail Games");
        map.insert(TileId::new(202, 1, 0), (1, 0).into(), Direction::A);
        map.insert_with_tokens(TileId::new(205, 1, 0), (0, 0).into(), Direction::A, vec![
            PlacedToken::new(TokenId::ChicaneWithLimit(Terrain::Gravel), (0.0, 0.0).into(), FloatDirection(3.0)),
            PlacedToken::new(TokenId::Chicane(Terrain::Gravel), (0.33, 0.0).into(), FloatDirection(0.0)),
        ]);
        let data = export_rgt(&map)
            .expect("Cannot export track data");
        assert_eq!(data, r#"{"Path":"C:\\Applications\\RallymanGT Track Editor\\My Tracks","Name":"Chicane Example 01","Author":"Holy Grail Games","Tuiles":[{"X":54,"Y":52,"Orientation":0,"TuileId":"202A"},{"X":55,"Y":52,"Orientation":0,"TuileId":"205A","Tokens":[{"X":0.0,"Y":0.0,"Orientation":180.0,"TokenId":"dirt1"},{"X":-0.5715768,"Y":0.0,"Orientation":0.0,"TokenId":"dirt2"}]}],"lowResWidth":2480,"lowResHeight":1748}"#);

        let mut map = Map::new();
        map.set_title("Chicane Example 02");
        map.set_author("Holy Grail Games");
        map.insert(TileId::new(202, 1, 0), (1, -1).into(), Direction::F);
        map.insert_with_tokens(TileId::new(205, 1, 0), (0, 0).into(), Direction::F, vec![
            PlacedToken::new(TokenId::ChicaneWithLimit(Terrain::Gravel), (0.0, 0.0).into(), FloatDirection(3.0)),
            PlacedToken::new(TokenId::Chicane(Terrain::Gravel), (0.33, 0.0).into(), FloatDirection(0.0)),
        ]);
        let data = export_rgt(&map)
            .expect("Cannot export track data");
        assert_eq!(data, r#"{"Path":"C:\\Applications\\RallymanGT Track Editor\\My Tracks","Name":"Chicane Example 02","Author":"Holy Grail Games","Tuiles":[{"X":54,"Y":51,"Orientation":5,"TuileId":"202A"},{"X":55,"Y":52,"Orientation":5,"TuileId":"205A","Tokens":[{"X":0.0,"Y":0.0,"Orientation":180.0,"TokenId":"dirt1"},{"X":-0.5715768,"Y":0.0,"Orientation":0.0,"TokenId":"dirt2"}]}],"lowResWidth":2480,"lowResHeight":1748}"#);
    }

    #[test]
    fn import_rgt_short_track() {
        let data = include_str!("tests/short-track2.rgt");
        let map = import_rgt(data)
            .expect("Cannot parse import file data");
        assert_eq!(map.title(), "ShortTrack2");
        assert_eq!(map.author(), "CarmLima");
        assert_abs_diff_eq!(map.tiles(), &[
            PlacedTile::new(TileId::new(102, 2, 0), ( 0,  1).into(), Direction::A),
            PlacedTile::new(TileId::new(106, 2, 2), ( 1,  1).into(), Direction::C),
            PlacedTile::new(TileId::new(116, 2, 2), ( 0,  0).into(), Direction::A),
            PlacedTile::new(TileId::new(117, 2, 2), ( 1,  0).into(), Direction::C),
            PlacedTile::new(TileId::new(111, 2, 2), ( 2, -1).into(), Direction::F),
            PlacedTile::new(TileId::new(125, 2, 1), (-1,  0).into(), Direction::A),
            PlacedTile::new(TileId::new(112, 2, 1), ( 2,  0).into(), Direction::B),
            PlacedTile::new(TileId::new(128, 2, 0), (-1, -1).into(), Direction::D),
            PlacedTile::new(TileId::new(138, 2, 1), (-2,  0).into(), Direction::D),
            PlacedTile::new(TileId::new(104, 2, 2), (-1,  1).into(), Direction::A),
            PlacedTile::new(TileId::new(105, 2, 1), (-2,  1).into(), Direction::D),
        ][..]);
    }

    #[test]
    fn export_rgt_short_track() {
        let mut map = Map::new();
        map.set_title("ShortTrack2");
        map.set_author("CarmLima");
        map.insert(TileId::new(102, 2, 0), ( 0,  1).into(), Direction::A);
        map.insert(TileId::new(106, 2, 2), ( 1,  1).into(), Direction::C);
        map.insert(TileId::new(116, 2, 2), ( 0,  0).into(), Direction::A);
        map.insert(TileId::new(117, 2, 2), ( 1,  0).into(), Direction::C);
        map.insert(TileId::new(111, 2, 2), ( 2, -1).into(), Direction::F);
        map.insert(TileId::new(125, 2, 1), (-1,  0).into(), Direction::A);
        map.insert(TileId::new(112, 2, 1), ( 2,  0).into(), Direction::B);
        map.insert(TileId::new(128, 2, 0), (-1, -1).into(), Direction::D);
        map.insert(TileId::new(138, 2, 1), (-2,  0).into(), Direction::D);
        map.insert(TileId::new(104, 2, 2), (-1,  1).into(), Direction::A);
        map.insert(TileId::new(105, 2, 1), (-2,  1).into(), Direction::D);
        let data = export_rgt(&map)
            .expect("Cannot export track data");
        assert_eq!(data, r#"{"Path":"C:\\Applications\\RallymanGT Track Editor\\My Tracks","Name":"ShortTrack2","Author":"CarmLima","Tuiles":[{"X":54,"Y":53,"Orientation":0,"TuileId":"102b"},{"X":53,"Y":53,"Orientation":2,"TuileId":"106b-2"},{"X":55,"Y":52,"Orientation":0,"TuileId":"116b-2"},{"X":54,"Y":52,"Orientation":2,"TuileId":"117b-2"},{"X":53,"Y":51,"Orientation":5,"TuileId":"111b-2"},{"X":56,"Y":52,"Orientation":0,"TuileId":"125b-1"},{"X":53,"Y":52,"Orientation":1,"TuileId":"112b-1"},{"X":56,"Y":51,"Orientation":3,"TuileId":"128b"},{"X":57,"Y":52,"Orientation":3,"TuileId":"138b-1"},{"X":55,"Y":53,"Orientation":0,"TuileId":"104b-2"},{"X":56,"Y":53,"Orientation":3,"TuileId":"105b-1"}],"lowResWidth":2480,"lowResHeight":1748}"#);
    }

    #[test]
    fn import_auto_short_track() {
        let data = include_str!("tests/short-track2.json");
        let native_map = import_auto(data)
            .expect("Cannot parse import file data");
        assert_eq!(native_map.title(), "ShortTrack2");
        assert_eq!(native_map.author(), "CarmLima");
        assert_eq!(native_map.tiles().len(), 11);

        let data = include_str!("tests/short-track2.rgt");
        let rgt_map = import_auto(data)
            .expect("Cannot parse import file data");
        assert_eq!(rgt_map.title(), native_map.title());
        assert_eq!(rgt_map.author(), native_map.author());
        assert_abs_diff_eq!(rgt_map.tiles(), native_map.tiles());

        let data = "{}";
        assert!(import_auto(data).is_err());
    }

    #[test]
    fn import_export_rx_track() {
        let data = include_str!("tests/rx-finland.rgt");
        let map = import_auto(data)
            .expect("Cannot parse import file data");
        assert_eq!(map.title(), "RX Finland");
        assert_eq!(map.author(), "");
        assert_eq!(map.tiles().len(), 16);

        let data = export_rgt(&map)
            .expect("Cannot export track data");
        assert_eq!(data, r#"{"Path":"C:\\Applications\\RallymanGT Track Editor\\My Tracks","Name":"RX Finland","Tuiles":[{"X":54,"Y":56,"Orientation":4,"TuileId":"403A"},{"X":54,"Y":55,"Orientation":1,"TuileId":"401A"},{"X":53,"Y":57,"Orientation":1,"TuileId":"119b"},{"X":54,"Y":58,"Orientation":2,"TuileId":"118b"},{"X":55,"Y":58,"Orientation":0,"TuileId":"414B"},{"X":54,"Y":57,"Orientation":0,"TuileId":"137b"},{"X":55,"Y":56,"Orientation":3,"TuileId":"126b"},{"X":55,"Y":55,"Orientation":4,"TuileId":"905A"},{"X":55,"Y":59,"Orientation":2,"TuileId":"206A"},{"X":56,"Y":59,"Orientation":4,"TuileId":"209B"},{"X":56,"Y":60,"Orientation":5,"TuileId":"402B"},{"X":57,"Y":58,"Orientation":5,"TuileId":"208A"},{"X":57,"Y":59,"Orientation":4,"TuileId":"209A"},{"X":57,"Y":60,"Orientation":1,"TuileId":"204A"},{"X":56,"Y":61,"Orientation":2,"TuileId":"410B"},{"X":56,"Y":58,"Orientation":3,"TuileId":"403B"}],"lowResWidth":2480,"lowResHeight":1748}"#);
    }

    #[test]
    fn file_name() {
        assert_eq!(&build_file_name(""), "MyTrack");
        assert_eq!(&build_file_name("\n"), "MyTrack");
        assert_eq!(&build_file_name("Short Track 2"), "Short Track 2");
        assert_eq!(&build_file_name("\n\tShort Track 2 "), "Short Track 2");
        assert_eq!(&build_file_name("</Short\\ Träck: 2>"), "_Short_ Träck 2");
    }
}

//----------------------------------------------------------------------------
