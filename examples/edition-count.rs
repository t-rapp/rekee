//----------------------------------------------------------------------------
// Example that reads a track file, and outputs the list of necessary editions
//----------------------------------------------------------------------------

use std::env;
use std::fmt::Write;
use std::fs;

use rekee::import;
use rekee::map::{Map, PlacedTileList};

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

    let editions = map.tiles().edition_summary();
    println!("= Count per edition =");
    for item in &editions {
        let mut count_info = String::new();
        if item.tile_count > 0 {
            let _ = write!(count_info, "{} tiles", item.tile_count);
        }
        if item.token_count > 0 {
            if !count_info.is_empty() {
                count_info.push_str(", ");
            }
            let _ = write!(count_info, "{} tokens", item.token_count);
        }
        if let Some(edition) = item.edition {
            println!("{:18}: {}x ({})", edition.to_string(), item.edition_count, &count_info);
        } else {
            println!("Undefined: {}", &count_info);
        }
    }

    Ok(())
}

//----------------------------------------------------------------------------
