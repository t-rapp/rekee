//----------------------------------------------------------------------------
// Example that reads a Rekee or RGT track file, and outputs an ordered list of used tiles
//----------------------------------------------------------------------------

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

    println!("= Count per tile =");
    for row in map.tiles().tile_summary() {
        println!("{:<4}: {}x", row.tile, row.count);
    }

    let total_count = map.tiles().len();
    println!("\nTotal: {} tiles", total_count);

    Ok(())
}

//----------------------------------------------------------------------------
