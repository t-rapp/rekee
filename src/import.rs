//----------------------------------------------------------------------------
// File: import.rs
// $Id$
//----------------------------------------------------------------------------

use std::fs::File;
use std::io::BufReader;

use serde::Deserialize;
use serde_json::Result;

use crate::hexagon::{Coordinate, Direction, Map, PlacedTile};

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

pub fn import_example(filename: &str) -> Result<Map> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let data: ImportRoot = serde_json::from_reader(reader)?;
    println!("{:?}", data);

    let mut map = Map::new();
    for tile in data.tiles {
        // convert from offset to axial coordinates
        let q = tile.x - (tile.y - (tile.y & 1)) / 2;
        let s = tile.y;
        let pos = Coordinate::new(s, q);
        let dir = Direction::from(tile.orientation + 1);
        println!("({}, {}), {} -> {}, {}, {}",
            tile.x, tile.y, tile.orientation, pos, dir, tile.id);
        map.insert(pos, PlacedTile { dir, tile: tile.id });
    }

    // realign tiles around center
    if !map.is_empty() {
        let mut min = (i32::MAX, i32::MAX);
        let mut max = (i32::MIN, i32::MIN);
        for pos in map.keys() {
            min.0 = min.0.min(pos.q());
            min.1 = min.1.min(pos.r());
            max.0 = max.0.max(pos.q());
            max.1 = max.1.max(pos.r());
        }
        let center_x = min.0 + (max.0 - min.0) / 2;
        let center_y = min.1 + (max.1 - min.1) / 2;
        let mut centered_map = Map::new();
        for (pos, tile) in map {
            let pos = Coordinate::new(pos.q() - center_x, pos.r() - center_y);
            centered_map.insert(pos, tile);
        }
        map = centered_map;
    }
    println!("{:?}", map);

    Ok(map)
}

//----------------------------------------------------------------------------
