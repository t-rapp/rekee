//----------------------------------------------------------------------------
// File: main.rs
// $Id$
//----------------------------------------------------------------------------

#![allow(dead_code)]

use std::io::Result;

use svg::Document;
use svg::node::element::{Group, Polyline};

mod hexagon;
use hexagon::*;

//----------------------------------------------------------------------------

fn points_to_string(points: &[Point]) -> String {
    let mut result = String::new();
    for p in points {
        result.push_str(&format!("{:.3},{:.3} ", p.0, p.1));
    }
    if points.len() > 2 {
        let p = points[0];
        result.push_str(&format!("{:.3},{:.3}", p.0, p.1));
    }
    result
}

//----------------------------------------------------------------------------

fn main() -> Result<()> {
    let mut document = Document::new()
        .set("width", 360)
        .set("height", 360);
    let mut group = Group::new()
        .set("transform", "scale(1, +1)");

    let layout = Layout::new(Orientation::pointy(), (30.0, 30.0), (180.0, 180.0));
    let hex = Coordinate::new(0, 0);
    let points = layout.hexagon_corners(hex);

    let line = Polyline::new()
        .set("id", "hex")
        .set("fill", "blue")
        .set("fill-opacity", 0.2)
        .set("stroke", "blue")
        .set("stroke-width", 1)
        .set("points", points_to_string(&points));
    group = group.add(line);
    document = document.add(group);
    svg::save("test01.svg", &document)?;

    Ok(())
}

//----------------------------------------------------------------------------
