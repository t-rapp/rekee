//----------------------------------------------------------------------------
// File: main.rs
// $Id$
//----------------------------------------------------------------------------

#![allow(dead_code)]

use std::io::Result;

use indoc::indoc;
use svg::Document;
use svg::node::element::{Definitions, Group, Image, Polygon, Style, Text, Use};

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
    let img = Image::new()
        .set("href", format!("images/thumb-{}.png", id))
        .set("width", 2.0 * size.0)
        .set("height", 2.0 * size.1)
        .set("transform", format!("rotate(-90) translate({:.3} {:.3})", -size.0, -size.1));
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

//----------------------------------------------------------------------------

fn main() -> Result<()> {
    let mut document = Document::new()
        .set("width", 600)
        .set("height", 600);

    let style = Style::new(indoc!(r"
        .label {
            font-family: sans;
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

    let layout = Layout::new(Orientation::pointy(), (50.0, 50.0), (300.0, 300.0));

    let defs = Definitions::new()
        .add(define_tile(&layout, "101a"))
        .add(define_tile(&layout, "102a"))
        .add(define_tile(&layout, "103a"))
        .add(define_tile(&layout, "104a"))
        .add(define_tile(&layout, "105a"))
        .add(define_tile(&layout, "106a"));
    document = document.add(defs);

    let mut group = Group::new();

    group = group.add(use_tile(&layout, "101a", (0, 0)));
    group = group.add(use_tile(&layout, "102a", (0, 1)));
    group = group.add(use_tile(&layout, "103a", (0, 2)));

    group = group.add(draw_hexagon(&layout, (0, 0), "hex"));
    group = group.add(draw_hexagon(&layout, (0, 1), "grid"));

    document = document.add(group);
    svg::save("test01.svg", &document)?;

    Ok(())
}

//----------------------------------------------------------------------------
