//----------------------------------------------------------------------------
// File: import.rs
// $Id$
//----------------------------------------------------------------------------

use log::{debug, trace};

use serde::Deserialize;
use serde_json::Result;

use crate::hexagon::*;
use crate::tile::*;

//----------------------------------------------------------------------------

#[derive(Debug, Default, Deserialize)]
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

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
#[serde(rename_all = "PascalCase")]
struct ImportTile {
    x: i32,
    y: i32,
    orientation: i32,
    #[serde(rename = "TuileId")]
    id: String,
}

pub fn import_rgt(data: &str) -> Result<Map> {
    let data: ImportRoot = serde_json::from_str(data)?;
    trace!("{:?}", data);

    let mut map = Map::new();
    for tile in data.tiles {
        // convert from offset to axial coordinates
        let q = -tile.x - ((tile.y & 1) + tile.y) / 2;
        let r = tile.y;
        let pos = Coordinate::new(q, r);
        let id = tile.id.parse::<TileId>()
            .unwrap_or_default();
        let dir = Direction::from(tile.orientation);
        debug!("import tile ({}, {}), {}, {} -> {}, {}, {}",
            tile.x, tile.y, tile.id, tile.orientation, pos, id, dir);
        map.insert(id, pos, dir);
    }
    map.align_center();
    trace!("{:?}", map);

    Ok(map)
}

//----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn import_short_track() {
        let data = include_str!("short-track2.rgt");
        let map = import_rgt(&data)
            .expect("Cannot parse import file data");
        let mut tiles = map.tiles().iter();
        assert_eq!(Some(&PlacedTile { id: TileId::new(102, 2, 0), pos: ( 0,  1).into(), dir: Direction::A }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: TileId::new(106, 2, 2), pos: ( 1,  1).into(), dir: Direction::C }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: TileId::new(116, 2, 2), pos: ( 0,  0).into(), dir: Direction::A }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: TileId::new(117, 2, 2), pos: ( 1,  0).into(), dir: Direction::C }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: TileId::new(111, 2, 2), pos: ( 2, -1).into(), dir: Direction::F }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: TileId::new(125, 2, 1), pos: (-1,  0).into(), dir: Direction::A }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: TileId::new(112, 2, 1), pos: ( 2,  0).into(), dir: Direction::B }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: TileId::new(128, 2, 0), pos: (-1, -1).into(), dir: Direction::D }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: TileId::new(138, 2, 1), pos: (-2,  0).into(), dir: Direction::D }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: TileId::new(104, 2, 2), pos: (-1,  1).into(), dir: Direction::A }), tiles.next());
        assert_eq!(Some(&PlacedTile { id: TileId::new(105, 2, 1), pos: (-2,  1).into(), dir: Direction::D }), tiles.next());
        assert_eq!(None, tiles.next());
    }
}

//----------------------------------------------------------------------------
