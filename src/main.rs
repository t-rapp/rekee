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

//----------------------------------------------------------------------------

fn draw_hexagon<'a, C, S>(layout: &Layout, hex: C, class: S) -> Polygon
    where C: Into<Coordinate>, S: Into<Option<&'a str>>
{
    let mut poly = Polygon::new();
    if let Some(class) = class.into() {
        poly = poly.set("class", class);
    }
    let corners = layout.hexagon_corners(hex.into());
    let points: Vec<String> = corners.iter()
        .map(|p| format!("{:.3},{:.3}", p.0, p.1))
        .collect();
    poly.set("points", points)
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

fn use_tile<C>(layout: &Layout, id: &str, hex: C) -> Use
    where C: Into<Coordinate>
{
    let pos = hex.into().to_pixel(&layout);
    Use::new()
        .set("href", format!("#{}", id))
        .set("x", pos.0)
        .set("y", pos.1)
}

fn draw_tile<C>(layout: &Layout, id: &str, hex: C) -> Group
    where C: Into<Coordinate>
{
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
    let pos = hex.into().to_pixel(&layout);
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

    let tiles = match args.get(1) {
        Some(val) => import::import_example(val)?,
        None => vec![
            ((0, 0).into(), "101a".to_string()),
            ((0, 1).into(), "102a".to_string()),
            ((1, 0).into(), "103a-1".to_string()),
        ],
    };

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
        .grid {
            fill: gray;
            fill-opacity: 0.02;
            stroke: gray;
            stroke-width: 0.4;
        }
        .hex {
            fill: none;
            stroke: blue;
            stroke-width: 1.4;
        }
        #logo {
            clip-path: polygon(93.3% 75.0%, 50.0% 100.0%, 6.7% 75.0%, 6.7% 25.0%, 50.0% 0.0%, 93.3% 25.0%);
        }"));
    document = document.add(style);

    let layout = Layout::new(Orientation::pointy(), Point(40.0, 40.0), Point(300.0, 300.0));

    let defs = Definitions::new();
    document = document.add(defs);

    let mut group = Group::new();
    let map_radius = 4;
    for q in -map_radius..=map_radius {
        let r1 = i32::max(-map_radius, -q - map_radius);
        let r2 = i32::min(map_radius, -q + map_radius);
        for r in r1..=r2 {
            group = group.add(draw_hexagon(&layout, (q, r), "grid"));
        }
    }

    for tile in &tiles {
        group = group.add(draw_tile(&layout, &tile.1, tile.0));
    }

    document = document.add(group);
    svg::save("test01.svg", &document)?;

    Ok(())
}

//----------------------------------------------------------------------------
