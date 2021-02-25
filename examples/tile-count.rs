//----------------------------------------------------------------------------
// Example that reads a Rekee JSON file and outputs an ordered list of tiles
//----------------------------------------------------------------------------

use std::collections::BTreeMap;
use std::env;
use std::fs;

use rekee::map::Map;

//----------------------------------------------------------------------------

fn load_file(filename: &str) -> Result<Map, String> {
    let data = fs::read_to_string(filename)
        .map_err(|err| format!("Cannot read track file: {}", err))?;
    let map = serde_json::from_str(&data)
        .map_err(|err| format!("Cannot parse track data: {}", err))?;
    Ok(map)
}

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    if let Some(filename) = args.get(1) {
        let map = load_file(filename)?;
        let mut list = BTreeMap::new();
        for tile in map.tiles() {
            let base_id = tile.id().base();
            let count = list.entry(base_id).or_insert(0);
            *count += 1;
        }
        println!("Count per tile:");
        for (id, count) in list.iter() {
            println!("{:<4}: {}", id.to_string(), count);
        }
    } else {
        // FIXME: better use CARGO_BIN_NAME here instead, but this seems to be unset for examples
        let program = env!("CARGO_CRATE_NAME");
        eprintln!("Usage: {} <input-file.json>", program);
    }
    Ok(())
}

//----------------------------------------------------------------------------
