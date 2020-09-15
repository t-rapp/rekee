//----------------------------------------------------------------------------
// File: main.rs
// $Id$
//----------------------------------------------------------------------------

#![allow(dead_code)]

use std::env;
use std::path::Path;

use getopts::Options;
use indoc::indoc;
use svg::Document;
use svg::node::element::{Definitions, Group, Image, Polygon, Style, Text, Use};

mod hexagon;
use hexagon::*;

mod import;
mod logger;

#[macro_use]
mod tile;
use tile::*;

//----------------------------------------------------------------------------

fn define_grid_hex(layout: &Layout) -> Polygon
{
    let corners = layout.hexagon_corners((0, 0).into());
    let points: Vec<String> = corners.iter()
        .map(|p| *p - layout.origin())
        .map(|p| format!("{},{}", p.x(), p.y()))
        .collect();
    Polygon::new()
        .set("id", "hex")
        .set("points", points)
}

fn use_grid_hex<C>(layout: &Layout, pos: C) -> Use
    where C: Into<Coordinate>
{
    let pos = pos.into().to_pixel(&layout);
    Use::new()
        .set("href", "#hex")
        .set("x", pos.x())
        .set("y", pos.y())
}

fn draw_label<C>(layout: &Layout, pos: C, text: &str) -> Text
    where C: Into<Coordinate>
{
    let pos = pos.into().to_pixel(&layout);
    Text::new()
        .set("class", "label")
        .set("x", pos.0)
        .set("y", pos.1)
        .add(svg::node::Text::new(text.to_string()))
}

fn define_tile(layout: &Layout, id: TileId) -> Group {
    let size = layout.size();
    let angle = Direction::A.to_angle(&layout);
    let img = Image::new()
        .set("href", format!("img/thumb-{}.png", id))
        .set("width", 2.0 * size.0)
        .set("height", 2.0 * size.1)
        .set("transform", format!("rotate({:.0}) translate({:.3} {:.3})", angle, -size.0, -size.1));
    let label = Text::new()
        .set("class", "label")
        .set("x", 0)
        .set("y", 0)
        .add(svg::node::Text::new(id.base().to_string()));
    Group::new()
        .set("id", id.to_string())
        .set("class", "tile")
        .add(img)
        .add(label)
}

fn use_tile<C, D>(layout: &Layout, id: TileId, pos: C, dir: D) -> Use
    where C: Into<Coordinate>, D: Into<Direction>
{
    let pos = pos.into().to_pixel(&layout);
    let angle = dir.into().to_angle(&layout);
    Use::new()
        .set("href", format!("#{}", id))
        .set("x", pos.0)
        .set("y", pos.1)
        .set("transform", format!("rotate({:.0} {:.3} {:.3})", angle, pos.0, pos.1))
}

fn draw_tile<C, D>(layout: &Layout, id: TileId, pos: C, dir: D) -> Group
    where C: Into<Coordinate>, D: Into<Direction>
{
    let size = layout.size();
    let angle = dir.into().to_angle(&layout);
    let img = Image::new()
        .set("href", format!("img/thumb-{}.png", id))
        .set("width", 2.0 * size.0)
        .set("height", 2.0 * size.1)
        .set("transform", format!("rotate({:.0}) translate({:.3} {:.3})", angle, -size.0, -size.1));
    let label = Text::new()
        .set("class", "label")
        .set("x", 0)
        .set("y", 0)
        .add(svg::node::Text::new(id.base().to_string()));
    let pos = pos.into().to_pixel(&layout);
    Group::new()
        .set("id", id.to_string())
        .set("class", "tile")
        .set("transform", format!("translate({:.3} {:.3})", pos.0, pos.1))
        .add(img)
        .add(label)
}

//----------------------------------------------------------------------------

const VERSION: &str = env!("CARGO_PKG_VERSION");
const PROGRAM: &str = env!("CARGO_PKG_NAME");

fn print_usage(program: &str, opts: &Options) {
    let brief = format!("Usage: {} [options] file.rgt", program);
    print!("{}", opts.usage(&brief));
}

fn main() {
    logger::init().unwrap();

    let args: Vec<String> = env::args().collect();
    let program = match Path::new(&args[0]).components().last() {
        Some(val) => val.as_os_str().to_str().unwrap(),
        None => PROGRAM,
    };

    let mut opts = Options::new();
    opts.optopt("o", "output", "write SVG output to file", "FILE");
    opts.optflag("", "pointy", "use pointy hexagon grid layout");
    opts.optflag("", "flat", "use flat hexagon grid layout");
    opts.optflagmulti("l", "left", "rotate map left");
    opts.optflagmulti("r", "right", "rotate map right");
    opts.optflag("", "version", "print version information");
    opts.optflag("h", "help", "print this help");

    let matches = match opts.parse(&args[1..]) {
        Ok(val) => val,
        Err(err) => {
            eprintln!("Error: Cannot parse program options: {}", err);
            return;
        }
    };
    if matches.opt_present("version") {
        println!("{}", VERSION);
        return;
    }
    if matches.opt_present("help") {
        print_usage(&program, &opts);
        return;
    }

    let output = matches.opt_str("output")
        .unwrap_or_else(|| "track.svg".to_string());
    let last_pointy_pos = matches.opt_positions("pointy").iter()
        .max().cloned();
    let last_flat_pos = matches.opt_positions("flat").iter()
        .max().cloned();
    let orientation = if last_pointy_pos >= last_flat_pos {
        Orientation::pointy()
    } else {
        Orientation::flat()
    };
    let layout = Layout::new(orientation, Point(40.0, 40.0), Point(320.0, 300.0));

    let mut map = Map::new();
    if let Some(file) = matches.free.get(0) {
        map = match import::import_example(file) {
            Ok(val) => val,
            Err(err) => {
                eprintln!("Error: Cannot import file data: {}", err);
                return;
            }
        }
    } else {
        let center = Coordinate::new(0, 0);
        map.insert(tile!(102, b), center, Direction::D);
        map.append(tile!(104, b), None);
        map.append(tile!(113, b), "R".parse().ok());
        map.append(tile!(117, b), "r".parse().ok());
        map.append(tile!(114, b), "R".parse().ok());
        map.append(tile!(115, b), "L".parse().ok());
        map.append(tile!(115, b), "l".parse().ok());
        map.append(tile!(108, b), "r".parse().ok());
        map.append(tile!(110, b), "L".parse().ok());
        map.append(tile!(107, b), "R".parse().ok());
        map.insert(tile!(101), (0, -2).into(), Direction::D);
        map.align_center();
    }
    for _ in 0..matches.opt_count("left") {
        map.rotate_left();
    }
    for _ in 0..matches.opt_count("right") {
        map.rotate_right();
    }

    let mut document = Document::new()
        .set("width", 2.0 * layout.origin().0)
        .set("height", 2.0 * layout.origin().1);

    let style = Style::new(indoc!(r"
        .label {
            font-family: sans-serif;
            font-size: 14px;
            font-weight: bold;
            fill: #444;
            paint-order: stroke;
            stroke: white;
            stroke-width: 2.0;
            dominant-baseline: middle;
            text-anchor: middle;
        }
        #hex {
            fill: gray;
            fill-opacity: 0.02;
            stroke: gray;
            stroke-width: 0.4;
        }
        #grid .label {
            fill-opacity: 0.6;
        }
        #logo {
            clip-path: polygon(93.3% 75.0%, 50.0% 100.0%, 6.7% 75.0%, 6.7% 25.0%, 50.0% 0.0%, 93.3% 25.0%);
        }"));
    document = document.add(style);

    let mut defs = Definitions::new();
    defs = defs.add(define_grid_hex(&layout));
    document = document.add(defs);

    let mut group = Group::new()
        .set("id", "grid");
    let map_radius = 4;
    for q in -map_radius..=map_radius {
        let r1 = i32::max(-map_radius, -q - map_radius);
        let r2 = i32::min(map_radius, -q + map_radius);
        for r in r1..=r2 {
            group = group.add(use_grid_hex(&layout, (q, r)));
        }
    }
    group = group.add(draw_label(&layout, (map_radius, 0), "+q"));
    group = group.add(draw_label(&layout, (-map_radius, 0), "-q"));
    group = group.add(draw_label(&layout, (0, map_radius), "+r"));
    group = group.add(draw_label(&layout, (0, -map_radius), "-r"));
    document = document.add(group);

    let mut group = Group::new()
        .set("id", "tiles");
    for tile in map.tiles() {
        group = group.add(draw_tile(&layout, tile.id, tile.pos, tile.dir));
    }
    document = document.add(group);

    match svg::save(&output, &document) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error: Cannot write SVG output file: {}", err);
            return;
        }
    }
}

//----------------------------------------------------------------------------
