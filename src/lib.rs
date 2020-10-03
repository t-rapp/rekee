//----------------------------------------------------------------------------
// File: lib.rs
// $Id$
//----------------------------------------------------------------------------

#![allow(dead_code)]

use wasm_bindgen::prelude::*;
use web_sys::{self, Document, Element};

mod hexagon;
use hexagon::*;

mod import;
mod logger;

mod tile;
use tile::*;

//----------------------------------------------------------------------------

type Result<T> = std::result::Result<T, JsValue>;

fn define_grid_hex(document: &Document, layout: &Layout) -> Result<Element>
{
    let corners = layout.hexagon_corners((0, 0).into());
    let points: Vec<String> = corners.iter()
        .map(|p| *p - layout.origin())
        .map(|p| format!("{},{}", p.x(), p.y()))
        .collect();
    let hex = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "polygon")?;
    hex.set_id("hex");
    hex.set_attribute("points", &points.join(" "))?;
    Ok(hex)
}

fn use_grid_hex<C>(document: &Document, layout: &Layout, pos: C) -> Result<Element>
    where C: Into<Coordinate>
{
    let pos = pos.into().to_pixel(&layout);
    let hex = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "use")?;
    hex.set_attribute("href", "#hex")?;
    hex.set_attribute("x", &pos.x().to_string())?;
    hex.set_attribute("y", &pos.y().to_string())?;
    Ok(hex)
}

fn draw_label<C>(document: &Document, layout: &Layout, pos: C, text: &str) -> Result<Element>
    where C: Into<Coordinate>
{
    let pos = pos.into().to_pixel(&layout);
    let label = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "text")?;
    label.set_attribute("class", "label")?;
    label.set_attribute("x", &pos.x().to_string())?;
    label.set_attribute("y", &pos.y().to_string())?;
    label.set_inner_html(text);
    Ok(label)
}

fn draw_tile<C, D>(document: &Document, layout: &Layout, id: TileId, pos: C, dir: D) -> Result<Element>
    where C: Into<Coordinate>, D: Into<Direction>
{
    let size = layout.size();
    let angle = dir.into().to_angle(&layout);
    let img = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "image")?;
    img.set_attribute("href", &format!("img/thumb-{}.png", id))?;
    img.set_attribute("width", &format!("{}", 2.0 * size.x()))?;
    img.set_attribute("height", &format!("{}", 2.0 * size.y()))?;
    img.set_attribute("transform", &format!("rotate({:.0}) translate({:.3} {:.3})", angle, -size.x(), -size.y()))?;

    let label = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "text")?;
    label.set_attribute("class", "label")?;
    label.set_attribute("x", "0")?;
    label.set_attribute("y", "0")?;
    label.set_inner_html(&id.base().to_string());

    let pos = pos.into().to_pixel(&layout);
    let tile = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "g")?;
    tile.set_attribute("id", &id.to_string())?;
    tile.set_attribute("class", "tile")?;
    tile.set_attribute("transform", &format!("translate({:.3} {:.3})", pos.0, pos.1))?;
    tile.append_child(&img)?;
    tile.append_child(&label)?;
    Ok(tile)
}

//----------------------------------------------------------------------------

#[wasm_bindgen]
pub fn main() -> Result<()> {
    logger::init().unwrap();

    let layout = Layout::new(Orientation::pointy(), Point(40.0, 40.0), Point(320.0, 300.0));
    let mut map = Map::new();
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

    let document = web_sys::window().unwrap().document().unwrap();
    let body = document.body().unwrap();

    let canvas = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "svg")?;
    canvas.set_id("canvas");
    canvas.set_attribute("width", &format!("{}", 2.0 * layout.origin().x()))?;
    canvas.set_attribute("height", &format!("{}", 2.0 * layout.origin().y()))?;
    body.append_child(&canvas)?;

    let defs = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "defs")?;
    defs.append_child(&define_grid_hex(&document, &layout)?.into())?;
    canvas.append_child(&defs)?;

    let group = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "g")?;
    group.set_id("grid");
    let map_radius = 4;
    for q in -map_radius..=map_radius {
        let r1 = i32::max(-map_radius, -q - map_radius);
        let r2 = i32::min(map_radius, -q + map_radius);
        for r in r1..=r2 {
            group.append_child(&use_grid_hex(&document, &layout, (q, r))?.into())?;
        }
    }
    group.append_child(&draw_label(&document, &layout, (map_radius, 0), "+q")?.into())?;
    group.append_child(&draw_label(&document, &layout, (-map_radius, 0), "-q")?.into())?;
    group.append_child(&draw_label(&document, &layout, (0, map_radius), "+r")?.into())?;
    group.append_child(&draw_label(&document, &layout, (0, -map_radius), "-r")?.into())?;
    canvas.append_child(&group)?;

    let group = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "g")?;
    group.set_id("tiles");
    for tile in map.tiles() {
        group.append_child(&draw_tile(&document, &layout, tile.id, tile.pos, tile.dir)?.into())?;
    }
    canvas.append_child(&group)?;

    Ok(())
}

//----------------------------------------------------------------------------
