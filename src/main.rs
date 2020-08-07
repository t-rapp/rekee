//----------------------------------------------------------------------------
// File: main.rs
// $Id$
//----------------------------------------------------------------------------

#![allow(dead_code)]

use std::io::Result;

use indoc::indoc;
use svg::Document;
use svg::node::element::{Group, Polygon, Style};

mod hexagon;
use hexagon::*;

//----------------------------------------------------------------------------

fn draw_hexagon<'a, S>(layout: &Layout, hex: Coordinate, class: S) -> Polygon
    where S: Into<Option<&'a str>>
{
    let mut poly = Polygon::new();
    if let Some(class) = class.into() {
        poly = poly.set("class", class);
    }
    let corners = layout.hexagon_corners(hex);
    let points: Vec<String> = corners.iter()
        .map(|p| format!("{:.3},{:.3}", p.0, p.1))
        .collect();
    poly.set("points", points)
}

//----------------------------------------------------------------------------

fn main() -> Result<()> {
    let mut document = Document::new()
        .set("width", 360)
        .set("height", 360);

    let style = Style::new(indoc!(r"
        .grid {
            fill: gray;
            fill-opacity: 0.2;
            stroke: gray;
            stroke-width: 1;
        }
        .hex {
            fill: blue;
            fill-opacity: 0.2;
            stroke: blue;
            stroke-width: 1;
        }"));
    document = document.add(style);

    let mut group = Group::new()
        .set("transform", "scale(1, +1)");

    let layout = Layout::new(Orientation::pointy(), (30.0, 30.0), (180.0, 180.0));
    group = group.add(draw_hexagon(&layout, (0, 0).into(), "hex"));
    group = group.add(draw_hexagon(&layout, (1, -1).into(), "grid"));
    group = group.add(draw_hexagon(&layout, (2, -1).into(), "grid"));
    group = group.add(draw_hexagon(&layout, (2, 0).into(), "grid"));
    group = group.add(draw_hexagon(&layout, (0, 2).into(), "grid"));

    document = document.add(group);
    svg::save("test01.svg", &document)?;

    Ok(())
}

//----------------------------------------------------------------------------
