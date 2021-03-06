//----------------------------------------------------------------------------
//! Import and export of map data.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use serde::{Deserialize, Serialize};
use serde_json::Result;

use crate::map::Map;
use crate::tile::*;
use super::*;

//----------------------------------------------------------------------------

#[derive(Debug, Default, Deserialize, Serialize)]
#[serde(default)]
#[serde(rename_all = "PascalCase")]
struct ImportRoot {
    path: String,
    name: String,
    #[serde(rename = "Tuiles")]
    tiles: Vec<ImportTile>,
    #[serde(rename = "lowResWidth")]
    low_res_width: u32,
    #[serde(rename = "lowResHeight")]
    low_res_height: u32,
}

#[derive(Debug, Default, Deserialize, Serialize)]
#[serde(default)]
#[serde(rename_all = "PascalCase")]
struct ImportTile {
    x: i32,
    y: i32,
    orientation: i32,
    #[serde(rename = "TuileId")]
    id: String,
}

const IMPORT_OFFSET: Coordinate = Coordinate::new(-81, 52);

pub fn import_rgt(data: &str) -> Result<Map> {
    let data: ImportRoot = serde_json::from_str(data)?;

    let mut map = Map::new();
    if !data.name.is_empty() {
        map.set_title(&data.name);
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
        let dir = Direction::from(tile.orientation);
        debug!("import tile ({}, {}), {}, {} -> {}, {}, {}",
            tile.x, tile.y, tile.id, tile.orientation, pos, id, dir);
        map.insert(id, pos, dir);
    }

    Ok(map)
}

pub fn export_rgt(map: &Map) -> Result<String> {
    // generate some default values for data that is not used in our editor
    let mut data = ImportRoot {
        path: "C:\\Applications\\RallymanGT Track Editor\\My Tracks".to_string(),
        name: map.title().to_string(),
        tiles: Vec::with_capacity(map.tiles().len()),
        low_res_width: 2480,
        low_res_height: 1748,
    };

    for tile in map.tiles() {
        let pos = tile.pos + IMPORT_OFFSET;
        let x = -pos.q() - ((pos.r() & 1) + pos.r()) / 2;
        let y = pos.r();
        let orientation: i32 = tile.dir.into();
        let id = match tile.id().num() {
            // special mapping for pit-stop tile
            101 => "101a".to_string(),
            _ => tile.id().to_string(),
        };
        debug!("export tile {}, {}, {} -> ({}, {}), {}, {}",
            tile.pos, tile.id(), tile.dir, x, y, &id, orientation);
        data.tiles.push(ImportTile { x, y, orientation, id });
    }

    serde_json::to_string(&data)
}

//----------------------------------------------------------------------------

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
    use crate::map::PlacedTile;
    use super::*;

    #[test]
    fn import_short_track() {
        let data = include_str!("short-track2.rgt");
        let map = import_rgt(&data)
            .expect("Cannot parse import file data");
        assert_eq!(map.title(), "ShortTrack2");
        let mut tiles = map.tiles().iter();
        assert_eq!(tiles.next(), Some(&PlacedTile::new(TileId::new(102, 2, 0), ( 0,  1).into(), Direction::A)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(TileId::new(106, 2, 2), ( 1,  1).into(), Direction::C)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(TileId::new(116, 2, 2), ( 0,  0).into(), Direction::A)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(TileId::new(117, 2, 2), ( 1,  0).into(), Direction::C)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(TileId::new(111, 2, 2), ( 2, -1).into(), Direction::F)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(TileId::new(125, 2, 1), (-1,  0).into(), Direction::A)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(TileId::new(112, 2, 1), ( 2,  0).into(), Direction::B)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(TileId::new(128, 2, 0), (-1, -1).into(), Direction::D)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(TileId::new(138, 2, 1), (-2,  0).into(), Direction::D)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(TileId::new(104, 2, 2), (-1,  1).into(), Direction::A)));
        assert_eq!(tiles.next(), Some(&PlacedTile::new(TileId::new(105, 2, 1), (-2,  1).into(), Direction::D)));
        assert_eq!(tiles.next(), None);
    }

    #[test]
    fn export_short_track() {
        let mut map = Map::new();
        map.set_title("ShortTrack2");
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
        assert_eq!(data, r#"{"Path":"C:\\Applications\\RallymanGT Track Editor\\My Tracks","Name":"ShortTrack2","Tuiles":[{"X":54,"Y":53,"Orientation":0,"TuileId":"102b"},{"X":53,"Y":53,"Orientation":2,"TuileId":"106b-2"},{"X":55,"Y":52,"Orientation":0,"TuileId":"116b-2"},{"X":54,"Y":52,"Orientation":2,"TuileId":"117b-2"},{"X":53,"Y":51,"Orientation":5,"TuileId":"111b-2"},{"X":56,"Y":52,"Orientation":0,"TuileId":"125b-1"},{"X":53,"Y":52,"Orientation":1,"TuileId":"112b-1"},{"X":56,"Y":51,"Orientation":3,"TuileId":"128b"},{"X":57,"Y":52,"Orientation":3,"TuileId":"138b-1"},{"X":55,"Y":53,"Orientation":0,"TuileId":"104b-2"},{"X":56,"Y":53,"Orientation":3,"TuileId":"105b-1"}],"lowResWidth":2480,"lowResHeight":1748}"#);
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
