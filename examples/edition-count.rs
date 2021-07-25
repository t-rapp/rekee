//----------------------------------------------------------------------------
// Example that reads a track file, and outputs the list of necessary editions
//----------------------------------------------------------------------------

use std::collections::BTreeMap;
use std::env;
use std::fs;

use rekee::import;
use rekee::map::Map;
use rekee::tile::TileList;

//----------------------------------------------------------------------------

fn load_file(filename: &str) -> Result<Map, String> {
    let data = fs::read_to_string(filename)
        .map_err(|err| format!("Cannot read track file: {}", err))?;
    let map = import::import_auto(&data)
        .map_err(|err| format!("Cannot parse track data: {}", err))?;
    Ok(map)
}

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    let map = if let Some(filename) = args.get(1) {
        load_file(filename)?
    } else {
        // FIXME: better use CARGO_BIN_NAME here instead, but this seems to be unset for examples
        let program = env!("CARGO_CRATE_NAME");
        eprintln!("Usage: {} <input-file.json>", program);
        return Ok(());
    };

    let mut map_tiles = BTreeMap::new();
    for tile in map.tiles() {
        let tile_num = tile.id().num();
        let count = map_tiles.entry(tile_num).or_insert(0);
        *count += 1;
    }
    let total_count = map.tiles().len();

    println!("= Count per Tile =");
    for (tile, count) in map_tiles.iter() {
        println!("{}: {}x", tile, count);
    }
    println!("Total: {} tiles\n", total_count);

    let editions = map.tiles().edition_summary();
    println!("= Count per Edition =");
    for item in &editions {
        if let Some(edition) = item.edition {
            println!("{:18}: {}x ({} tiles)", edition.to_string(), item.edition_count, item.tile_count);
        } else {
            println!("Undefined: {} tiles", item.tile_count);
        }
    }

    Ok(())
}

//----------------------------------------------------------------------------
