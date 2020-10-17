//----------------------------------------------------------------------------
//! Library providing track editor functions for the Rekee web application.
//
// $Id$
//----------------------------------------------------------------------------

#![allow(dead_code)]

use wasm_bindgen::prelude::*;

mod controller;
use controller::*;

pub mod hexagon;
use hexagon::*;

mod import;
mod logger;
mod tile;

mod view;
use view::*;

//----------------------------------------------------------------------------

#[wasm_bindgen]
pub fn main() -> Result<(), JsValue> {
    logger::init().unwrap();

    let document = web_sys::window().unwrap().document().unwrap();
    let parent = document.get_element_by_id("main").
        ok_or_else(|| "Cannot find '#main' parent element for page")?;

    let layout = Layout::new(Orientation::pointy(), Point(40.0, 40.0), Point(320.0, 300.0));
    controller::init(PageView::new(parent, layout)?);

    nuts::publish(InsertTileEvent { id: tile!(102, b), pos: (0, 0).into(), dir: Direction::D });
    nuts::publish(AppendTileEvent { id: tile!(104, b), hint: None });
    nuts::publish(AppendTileEvent { id: tile!(113, b), hint: "R".parse().ok() });
    nuts::publish(AppendTileEvent { id: tile!(117, b), hint: "r".parse().ok() });
    nuts::publish(AppendTileEvent { id: tile!(114, b), hint: "R".parse().ok() });
    nuts::publish(AppendTileEvent { id: tile!(115, b), hint: "L".parse().ok() });
    nuts::publish(AppendTileEvent { id: tile!(115, b), hint: "l".parse().ok() });
    nuts::publish(AppendTileEvent { id: tile!(108, b), hint: "r".parse().ok() });
    nuts::publish(AppendTileEvent { id: tile!(110, b), hint: "L".parse().ok() });
    nuts::publish(AppendTileEvent { id: tile!(107, b), hint: "R".parse().ok() });
    nuts::publish(InsertTileEvent { id: tile!(101), pos: (0, -2).into(), dir: Direction::D});
    nuts::publish(AlignCenterEvent);

    Ok(())
}

//----------------------------------------------------------------------------
