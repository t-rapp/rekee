//----------------------------------------------------------------------------
// File: import.rs
// $Id$
//----------------------------------------------------------------------------

use std::fs::File;
use std::io::BufReader;

use serde::Deserialize;
use serde_json::Result;

use crate::hexagon::Coordinate;

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

pub fn import_example(filename: &str) -> Result<Vec<(Coordinate, String)>> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let data: ImportRoot = serde_json::from_reader(reader)?;
    println!("{:?}", data);

    let mut tiles: Vec<_> = data.tiles.iter()
        .map(|tile| {
            let q = tile.x - (tile.y - (tile.y & 1)) / 2;
            let s = tile.y;
            println!("({}, {}) -> ({}, {}) {}", tile.x, tile.y, s, q, tile.id);
            (Coordinate::new(s, q), tile.id.clone())
        })
        .collect();

    // realign tiles around center
    if !tiles.is_empty() {
        let mut min = (i32::MAX, i32::MAX);
        let mut max = (i32::MIN, i32::MIN);
        for tile in tiles.iter() {
            min.0 = min.0.min(tile.0.q());
            min.1 = min.1.min(tile.0.r());
            max.0 = max.0.max(tile.0.q());
            max.1 = max.1.max(tile.0.r());
        }
        let center_x = min.0 + (max.0 - min.0) / 2;
        let center_y = min.1 + (max.1 - min.1) / 2;
        for tile in tiles.iter_mut() {
            tile.0 = Coordinate::new(tile.0.q() - center_x, tile.0.r() - center_y);
        }
    }

    tiles.sort_unstable();
    println!("{:?}", tiles);

    Ok(tiles)
}

//----------------------------------------------------------------------------
