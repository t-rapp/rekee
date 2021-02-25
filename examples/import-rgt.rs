//----------------------------------------------------------------------------
// Example that reads a file in RGT format and outputs Rekee JSON
//----------------------------------------------------------------------------

use std::env;
use std::fs;

use rekee::import;
use rekee::map::Map;

//----------------------------------------------------------------------------

fn import_file(filename: &str) -> Result<Map, String> {
    let data = fs::read_to_string(filename)
        .map_err(|err| format!("Cannot read RGT file: {}", err))?;
    let mut map = import::import_rgt(&data)
        .map_err(|err| format!("Cannot import RGT data: {}", err))?;
    map.align_center();
    Ok(map)
}

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    if let Some(filename) = args.get(1) {
        let map = import_file(filename)?;
        let text = serde_json::to_string_pretty(&map).unwrap();
        println!("{}", &text);
    } else {
        // FIXME: better use CARGO_BIN_NAME here instead, but this seems to be unset for examples
        let program = env!("CARGO_CRATE_NAME");
        eprintln!("Usage: {} <input-file.rgt>", program);
    }
    Ok(())
}

//----------------------------------------------------------------------------
