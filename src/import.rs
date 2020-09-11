//----------------------------------------------------------------------------
// File: import.rs
// $Id$
//----------------------------------------------------------------------------

use std::fs::File;
use std::io::BufReader;

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

pub fn import_example(filename: &str) -> Result<Map> {
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(file);
    let data: ImportRoot = serde_json::from_reader(reader)?;
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
