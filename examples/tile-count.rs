//----------------------------------------------------------------------------
// Example that reads a Rekee or RGT track file, and outputs an ordered list of used tiles
//----------------------------------------------------------------------------

use std::env;
use std::fs;

use rekee::import;
use rekee::map::Map;
use rekee::tile::TileList;
use rekee::token::TokenList;

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

    let mut tokens = Vec::new();
    for tile in map.tiles() {
        tokens.extend_from_slice(&tile.tokens);
    }
    if !tokens.is_empty() {
        println!("\n= Count per token =");
        for row in tokens.token_summary() {
            println!("{}: {}x", row.token, row.count);
        }
    }

    let tile_count = map.tiles().len();
    let token_count = tokens.len();
    print!("\nTotal: {} tiles", tile_count);
    if token_count > 0 {
        print!(", {} tokens", token_count);
    }
    println!();

    Ok(())
}

//----------------------------------------------------------------------------
