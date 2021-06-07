//----------------------------------------------------------------------------
// Example that reads a track file, and outputs the list of necessary editions
//----------------------------------------------------------------------------

use std::cmp::Reverse;
use std::collections::BTreeMap;
use std::collections::btree_map::Iter;
use std::env;
use std::fs;

use rekee::edition::Edition;
use rekee::import;
use rekee::map::Map;

//----------------------------------------------------------------------------

struct TileCount {
    inner: BTreeMap::<u16, u32>,
}

impl TileCount {
    pub fn from_edition(edition: Edition) -> Self {
        let mut list = BTreeMap::new();
        for tile in edition.tiles() {
            if tile.side() > 1 {
                continue; // only count each tile once per a/b side
            }
            let tile_num = tile.num();
            let count = list.entry(tile_num).or_insert(0);
            *count += 1;
        }
        TileCount { inner: list }
    }

    pub fn from_map(map: &Map) -> Self {
        let mut list = BTreeMap::new();
        for tile in map.tiles() {
            let tile_num = tile.id().num();
            let count = list.entry(tile_num).or_insert(0);
            *count += 1;
        }
        TileCount { inner: list }
    }

    pub fn iter(&self) -> Iter<u16, u32> {
        self.inner.iter()
    }

    pub fn intersection(&self, other: &Self) -> Self {
        let mut list = BTreeMap::new();
        for (tile, count) in self.inner.iter() {
            if let Some(other_count) = other.inner.get(tile) {
                let count = count.min(other_count);
                if *count > 0 {
                    list.insert(*tile, *count);
                }
            }
        }
        TileCount { inner: list }
    }

    pub fn decrement(&mut self, tile: u16, value: u32) -> u32 {
        if let Some(count) = self.inner.get_mut(&tile) {
            let delta = value.min(*count);
            *count -= delta;
            delta
        } else {
            0
        }
    }

    pub fn total_count(&self) -> u32 {
        let mut total_count = 0;
        for (_, count) in self.inner.iter() {
            total_count += count;
        }
        total_count
    }
}

//----------------------------------------------------------------------------

struct EditionTileCount {
    edition: Edition,
    common: TileCount,
    count: u32,
    tiles_used: u32,
}

impl EditionTileCount {
    pub fn new(edition: Edition, map_tiles: &TileCount) -> Self {
        let common = TileCount::from_edition(edition)
            .intersection(&map_tiles);
        EditionTileCount { edition, common, count: 0, tiles_used: 0 }
    }
}

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

    let mut map_tiles = TileCount::from_map(&map);
    println!("= Count per Tile =");
    for (tile, count) in map_tiles.iter() {
        println!("{}: {}x", tile, count);
    }
    println!("Total: {} tiles\n", map_tiles.total_count());

    let mut editions: Vec<_> = Edition::iter()
        .map(|item| EditionTileCount::new(*item, &map_tiles))
        .collect();
    editions.sort_by_cached_key(|item| Reverse(item.common.total_count()));
    loop {
        let mut edition_used = false;
        for item in editions.iter_mut() {
            let mut tiles_used = 0;
            for (&tile, &count) in item.common.iter() {
                let delta = map_tiles.decrement(tile, count);
                tiles_used += delta;
            }
            if tiles_used > 0 {
                item.count += 1;
                item.tiles_used += tiles_used;
                edition_used = true;
            }
        }
        if !edition_used {
            break;
        }
    }
    editions.sort_unstable_by_key(|item| item.edition);

    println!("= Count per Edition =");
    for item in &editions {
        if item.count > 0 {
            println!("{:18}: {}x ({} tiles)", item.edition.to_string(), item.count, item.tiles_used);
        }
    }
    let undefined_count = map_tiles.total_count();
    if undefined_count > 0 {
        println!("Undefined: {} tiles", undefined_count);
    }

    Ok(())
}

//----------------------------------------------------------------------------
