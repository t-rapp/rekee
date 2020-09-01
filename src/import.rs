//----------------------------------------------------------------------------
// File: import.rs
// $Id$
//----------------------------------------------------------------------------

use std::fs::File;
use std::io::BufReader;

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
        let id = tile.id.parse::<TileId>()
            .unwrap_or_default();
        let dir = Direction::from(tile.orientation + 1);
        println!("({}, {}), {}, {} -> {}, {}, {}",
            tile.x, tile.y, tile.id, tile.orientation, pos, id, dir);
        map.insert(pos, PlacedTile { id, dir });
    }

    // realign tiles around center
    if !map.is_empty() {
        let layout = Layout::new(Orientation::pointy(), Point(1.0, 1.0), Point(0.0, 0.0));
        let mut min = (f32::MAX, f32::MAX);
        let mut max = (f32::MIN, f32::MIN);
        for pos in map.keys() {
            let p = pos.to_pixel(&layout);
            min.0 = min.0.min(p.x());
            min.1 = min.1.min(p.y());
            max.0 = max.0.max(p.x());
            max.1 = max.1.max(p.y());
        }
        let center_x = min.0 + (max.0 - min.0) / 2.0;
        let center_y = min.1 + (max.1 - min.1) / 2.0;
        let center = Coordinate::from_pixel_rounded(&layout, Point(center_x, center_y));
        let mut centered_map = Map::new();
        for (pos, tile) in map {
            let pos = pos - center;
            centered_map.insert(pos, tile);
        }
        map = centered_map;
    }
    println!("{:?}", map);

    Ok(map)
}

//----------------------------------------------------------------------------
