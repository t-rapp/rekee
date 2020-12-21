//----------------------------------------------------------------------------
//! Rendering of objects and common functions.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
// $Id$
//----------------------------------------------------------------------------

#![allow(clippy::needless_return)]

use indoc::indoc;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Document, Element};

use crate::hexagon::*;
use crate::tile::*;

mod catalog;
mod map;
mod welcome;

//----------------------------------------------------------------------------

pub type CatalogView = catalog::CatalogView;

pub type MapView = map::MapView;

pub type WelcomeView = welcome::WelcomeView;

type Result<T> = std::result::Result<T, JsValue>;

//----------------------------------------------------------------------------

const SVG_NS: Option<&str> = Some(SVG_NS_STR);
const SVG_NS_STR: &str = "http://www.w3.org/2000/svg";

const TILE_STYLE: &str = indoc!(r#"
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
        user-select: none;
        pointer-events: none;
    }"#);

//----------------------------------------------------------------------------

fn draw_tile<C, D>(document: &Document, layout: &Layout, id: TileId, pos: C, dir: D) -> Result<Element>
    where C: Into<Coordinate>, D: Into<Direction>
{
    let size = layout.size();
    let angle = dir.into().to_angle(&layout);
    let img = document.create_element_ns(SVG_NS, "image")?;
    img.set_attribute("href", &format!("img/thumb-{}.png", id))?;
    img.set_attribute("width", &format!("{}", 2.0 * size.x()))?;
    img.set_attribute("height", &format!("{}", 2.0 * size.y()))?;
    img.set_attribute("transform", &format!("rotate({:.0}) translate({:.3} {:.3})", angle, -size.x(), -size.y()))?;

    let label = document.create_element_ns(SVG_NS, "text")?;
    label.set_attribute("class", "label")?;
    label.set_attribute("x", "0")?;
    label.set_attribute("y", "0")?;
    let text = id.base().to_string();
    label.append_child(&document.create_text_node(&text))?;

    let pos = pos.into().to_pixel(&layout);
    let tile = document.create_element_ns(SVG_NS, "g")?;
    tile.set_attribute("id", &id.to_string())?;
    tile.set_attribute("class", "tile")?;
    tile.set_attribute("transform", &format!("translate({:.3} {:.3})", pos.x(), pos.y()))?;
    tile.append_child(&img)?;
    tile.append_child(&label)?;
    Ok(tile)
}

fn mouse_position(event: &web_sys::MouseEvent) -> Option<Point> {
    let element = event.current_target()
        .and_then(|target| target.dyn_into::<web_sys::Element>().ok())?;
    let rect = element.get_bounding_client_rect();
    let x = event.client_x() as f32 - rect.left() as f32;
    let y = event.client_y() as f32 - rect.top() as f32;
    Some(Point(x, y))
}

// Helper macro that checks an option result and aborts the current function in case of an error.
#[doc(hidden)]
#[macro_export]
macro_rules! check {
    ($e:expr) => {
        match $e {
            None => return,
            Some(val) => val,
        }
    };
}

//----------------------------------------------------------------------------

trait ElementHidden {
    fn set_hidden(&self, value: bool);
}

impl<T: AsRef<Element>> ElementHidden for T {
    fn set_hidden(&self, value: bool) {
        let elm = self.as_ref();
        if value {
            check!(elm.class_list().add_1("is-hidden").ok());
        } else {
            check!(elm.class_list().remove_1("is-hidden").ok());
        }
    }
}

//----------------------------------------------------------------------------
