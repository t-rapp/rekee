//----------------------------------------------------------------------------
//! Library providing track editor functions for the Rekee web application.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use wasm_bindgen::prelude::*;

mod controller;
use controller::*;

pub mod hexagon;
use hexagon::*;

mod import;
pub mod edition;
mod logger;

#[macro_use]
pub mod tile;

#[macro_use]
mod view;
use view::*;

//----------------------------------------------------------------------------

#[wasm_bindgen]
pub fn main() -> Result<(), JsValue> {
    logger::init().unwrap();

    let document = web_sys::window().unwrap().document().unwrap();
    let layout = Layout::new(Orientation::pointy(), Point(50.0, 50.0), Point(450.0, 400.0));

    let parent = document.get_element_by_id("catalog-container")
        .ok_or("Cannot find '#catalog-container' parent element for catalog component")?;
    CatalogController::init(CatalogView::new(parent, &layout)?);

    let parent = document.get_element_by_id("map-container")
        .ok_or("Cannot find '#map-container' parent element for map component")?;
    MapController::init(MapView::new(parent, layout)?);

    VersionView::new(&document)?;

    // build some example track when compiling in development mode
    if cfg!(debug_assertions) {
        nuts::publish(InsertTileEvent { id: tile!(102, b), pos: (0, 0).into(), dir: Direction::D });
        nuts::publish(AppendTileEvent { id: tile!(104, b), pos: Some((-1, 0).into()), hint: None });
        nuts::publish(AppendTileEvent { id: tile!(113, b), pos: None, hint: "R".parse().ok() });
        nuts::publish(AppendTileEvent { id: tile!(117, b), pos: None, hint: "r".parse().ok() });
        nuts::publish(AppendTileEvent { id: tile!(114, b), pos: None, hint: "R".parse().ok() });
        nuts::publish(AppendTileEvent { id: tile!(115, b), pos: None, hint: None });
        nuts::publish(AppendTileEvent { id: tile!(115, b), pos: None, hint: None });
        nuts::publish(AppendTileEvent { id: tile!(108, b), pos: None, hint: "r".parse().ok() });
        nuts::publish(AppendTileEvent { id: tile!(110, b), pos: None, hint: "L".parse().ok() });
        nuts::publish(AppendTileEvent { id: tile!(107, b), pos: None, hint: None });
        nuts::publish(InsertTileEvent { id: tile!(101), pos: (0, -2).into(), dir: Direction::D});
        nuts::publish(AlignCenterEvent);
    }

    // initialize after all other controllers are completed to ignore events during setup
    WelcomeController::init(WelcomeView::new(&document)?);

    Ok(())
}

//----------------------------------------------------------------------------
