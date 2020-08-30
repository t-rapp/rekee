//----------------------------------------------------------------------------
// File: main.rs
// $Id$
//----------------------------------------------------------------------------

#![allow(dead_code)]

use std::io::Result;

use indoc::indoc;
use svg::Document;
use svg::node::element::{Definitions, Group, Image, Polygon, Style, Text, Use};

mod import;

mod hexagon;
use hexagon::*;

mod tile;

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

fn define_tile(layout: &Layout, id: &str) -> Group {
    let size = layout.size();
    let angle = Direction::A.to_angle(&layout) - 60.0;
    let img = Image::new()
        .set("href", format!("img/thumb-{}.png", id))
        .set("width", 2.0 * size.0)
        .set("height", 2.0 * size.1)
        .set("transform", format!("rotate({:.0}) translate({:.3} {:.3})", angle, -size.0, -size.1));
    let label = Text::new()
        .set("class", "label")
        .set("x", 0)
        .set("y", 0)
        .add(svg::node::Text::new(id.to_string()));
    Group::new()
        .set("id", id)
        .set("class", "tile")
        .add(img)
        .add(label)
}

fn use_tile<C, D>(layout: &Layout, pos: C, dir: D, id: &str) -> Use
    where C: Into<Coordinate>, D: Into<Direction>
{
    let pos = pos.into().to_pixel(&layout);
    let angle = dir.into().to_angle(&layout) - 60.0;
    Use::new()
        .set("href", format!("#{}", id))
        .set("x", pos.0)
        .set("y", pos.1)
        .set("transform", format!("rotate({:.0} {:.3} {:.3})", angle, pos.0, pos.1))
}

fn draw_tile<C, D>(layout: &Layout, pos: C, dir: D, id: &str) -> Group
    where C: Into<Coordinate>, D: Into<Direction>
{
    let size = layout.size();
    let angle = dir.into().to_angle(&layout) - 60.0;
    let img = Image::new()
        .set("href", format!("img/thumb-{}.png", id))
        .set("width", 2.0 * size.0)
        .set("height", 2.0 * size.1)
        .set("transform", format!("rotate({:.0}) translate({:.3} {:.3})", angle, -size.0, -size.1));
    let label = Text::new()
        .set("class", "label")
        .set("x", 0)
        .set("y", 0)
        .add(svg::node::Text::new(id.to_string()));
    let pos = pos.into().to_pixel(&layout);
    Group::new()
        .set("id", id)
        .set("class", "tile")
        .set("transform", format!("translate({:.3} {:.3})", pos.0, pos.1))
        .add(img)
        .add(label)
}

//----------------------------------------------------------------------------

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();

    let mut map = match args.get(1) {
        Some(val) => import::import_example(val)?,
        None => Map::new(),
    };

    if map.is_empty() {
        map.insert((0, 0).into(), PlacedTile { dir: Direction::A, tile: "101a".to_string() });
        map.insert((0, 1).into(), PlacedTile { dir: Direction::A, tile: "102a".to_string() });
        map.insert((1, 0).into(), PlacedTile { dir: Direction::A, tile: "103a-1".to_string() });
    }

    let mut document = Document::new()
        .set("width", 600)
        .set("height", 600);

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
        #grid {
            fill: gray;
            fill-opacity: 0.02;
            stroke: gray;
            stroke-width: 0.4;
        }
        #logo {
            clip-path: polygon(93.3% 75.0%, 50.0% 100.0%, 6.7% 75.0%, 6.7% 25.0%, 50.0% 0.0%, 93.3% 25.0%);
        }"));
    document = document.add(style);

    let layout = Layout::new(Orientation::pointy(), Point(40.0, 40.0), Point(300.0, 300.0));

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
    document = document.add(group);

    let mut group = Group::new()
        .set("id", "tiles");
    for (pos, tile) in map {
        group = group.add(draw_tile(&layout, pos, tile.dir, &tile.tile));
    }
    document = document.add(group);

    svg::save("test01.svg", &document)?;
    Ok(())
}

//----------------------------------------------------------------------------
