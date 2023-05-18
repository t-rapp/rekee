//----------------------------------------------------------------------------
// Reads a file in Rekee JSON or RGT format and generates a SVG image
//----------------------------------------------------------------------------

use std::env;
use std::fs;

use getopts::Options;
use indoc::indoc;
use rekee::export::ExportScale;
use rekee::export::svg::{TileImageGroup, TileLabelGroup, TokenImageGroup};
use rekee::export::svg::{BASE_URL_PATTERN, TILE_ID_PATTERN, TOKEN_ID_PATTERN};
use rekee::hexagon::{Layout, Orientation, Rect};
use rekee::import;
use rekee::map::Map;
use svg::Document;
use svg::node::element::{Group, Style};

//----------------------------------------------------------------------------

fn import_file(filename: &str) -> Result<Map, String> {
    let data = fs::read_to_string(filename)
        .map_err(|err| format!("Cannot read track file: {}", err))?;
    let mut map = import::import_auto(&data)
        .map_err(|err| format!("Cannot import track data: {}", err))?;
    map.align_center();
    Ok(map)
}

//----------------------------------------------------------------------------

const MAP_PADDING: f32 = 15.0;

const TILE_STYLE: &str = indoc!("
    .label {
        font-family: Overpass, sans-serif;
        font-size: {font-size};
        font-variant-numeric: tabular-nums;
        font-weight: bold;
        fill: #444;
        paint-order: stroke;
        stroke: white;
        stroke-width: 2.0;
        dominant-baseline: middle;
        text-anchor: middle;
    }");

//----------------------------------------------------------------------------

fn print_usage(program: &str, opts: &Options) {
    let brief = format!("Usage: {} [options] <input-file.rgt>", program);
    print!("{}", opts.usage(&brief));
    println!();
    println!("Pattern Strings:");
    println!("    {:<12} base path or URL for tile and token images", BASE_URL_PATTERN);
    println!("    {:<12} tile identifier (like \"102a-1\")", TILE_ID_PATTERN);
    println!("    {:<12} token identifier (like \"jump-gravel\")", TOKEN_ID_PATTERN);
    println!();
    println!("For example when using the following command options:");
    println!("");
    println!("    {} --base-url \"http://example/path\" \\", program);
    println!("        --tile-image-url \"{}/tile-{}.png\" \\", BASE_URL_PATTERN, TILE_ID_PATTERN);
    println!("        --token-image-url \"{}/tokens/{}.png\"", BASE_URL_PATTERN, TOKEN_ID_PATTERN);
    println!("");
    println!("The full image URL of tile 204a will be:");
    println!("    \"http://example/path/tile-204a.png\"");
    println!("and the full image URL of a jump token on gravel will be:");
    println!("    \"http://example/path/tokens/jump-gravel.png\"");
}

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();

    // CARGO_BIN_NAME is only available for example targets with Cargo v1.69 or later
    let program: &str = option_env!("CARGO_BIN_NAME")
        .unwrap_or(env!("CARGO_CRATE_NAME"));

    let mut opts = Options::new();
    opts.optopt("b", "base-url", "base path for tile and token images URLs", "STR");
    opts.optopt("t", "tile-image-url", "filename pattern for tile image URLs", "PATTERN");
    opts.optopt("T", "token-image-url", "filename pattern for token image URLs", "PATTERN");
    opts.optopt("o", "output", "write SVG output to file", "FILE");
    opts.optopt("s", "scale", "output scale (small, medium, large, extra-large)", "STR");
    opts.optflag("", "version", "print version information");
    opts.optflag("h", "help", "print this help");

    let matches = opts.parse(&args[1..])
        .map_err(|err| format!("Cannot parse program options: {}", err))?;
    if matches.opt_present("version") {
        println!("{}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }
    if matches.opt_present("help") {
        print_usage(program, &opts);
        return Ok(());
    }

    let base_url = matches.opt_str("base-url")
        .unwrap_or_else(|| "http://rekee.net".to_string());
    let tile_image_url = matches.opt_str("tile-image-url")
        .unwrap_or_else(|| format!("{}/tiles/tile-{}.png", BASE_URL_PATTERN, TILE_ID_PATTERN))
        .replace(BASE_URL_PATTERN, &base_url);
    let token_image_url = matches.opt_str("token-image-url")
        .unwrap_or_else(|| format!("{}/tokens/{}.png", BASE_URL_PATTERN, TOKEN_ID_PATTERN))
        .replace(BASE_URL_PATTERN, &base_url);
    let output = matches.opt_str("output")
        .unwrap_or_else(|| "track.svg".to_string());
    let scale = matches.opt_get_default("scale", ExportScale::default())
        .map_err(|err| format!("Invalid scale option value: {}", err))?;
    let filename = match matches.free.get(0) {
        Some(val) => val,
        None => {
            eprintln!("Usage: {} [options] <input-file.rgt>", program);
            return Err("Missing input file name".into());
        }
    };

    let map = import_file(filename)?;
    let tile_size = scale.tile_size();
    let layout = Layout::new(Orientation::pointy(), tile_size, tile_size * 1.5);

    // calculate rectangular map area that is covered with tiles
    let mut map_area = Rect::new(f32::NAN, f32::NAN, 0.0, 0.0);
    for tile in map.tiles() {
        map_area = map_area.union(&layout.hexagon_rect(tile.pos));
    }
    map_area = map_area.with_padding(MAP_PADDING);
    map_area.left = map_area.left.floor();
    map_area.top = map_area.top.floor();
    map_area.width = map_area.width.ceil();
    map_area.height = map_area.height.ceil();

    let width = (2.0 * layout.origin().x()).round();
    let height = (2.0 * layout.origin().y()).round();
    let canvas_viewbox = Rect::new(0.0, 0.0, width, height)
        .union(&map_area);

    let mut document = Document::new()
        .set("width", format!("{:.0}px", canvas_viewbox.width))
        .set("height", format!("{:.0}px", canvas_viewbox.height))
        .set("viewBox", format!("{:.0} {:.0} {:.0} {:.0}",
            canvas_viewbox.left, canvas_viewbox.top,
            canvas_viewbox.width, canvas_viewbox.height));

    let text = TILE_STYLE
        .replace("{font-size}", &format!("{}px", scale.tile_label_height()));
    let style = Style::new(text);
    document = document.add(style);

    let mut group = Group::new()
        .set("class", "tiles");
    for tile in map.tiles() {
        group = group.add(Group::from(TileImageGroup::new(&layout, tile, &tile_image_url)));
    }
    document = document.add(group);

    let mut group = Group::new()
        .set("class", "tokens");
    for tile in map.tiles() {
        for token in &tile.tokens {
            group = group.add(Group::from(TokenImageGroup::new(&layout, tile, token, &token_image_url)));
        }
    }
    document = document.add(group);

    let mut group = Group::new()
        .set("class", "labels");
    for tile in map.tiles() {
        group = group.add(Group::from(TileLabelGroup::new(&layout, tile)));
    }
    document = document.add(group);

    svg::save(output, &document)
        .map_err(|err| format!("Cannot write SVG output file: {}", err))
}

//----------------------------------------------------------------------------
