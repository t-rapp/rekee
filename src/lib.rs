//----------------------------------------------------------------------------
//! Library providing track editor functions for the Rekee web application.
//!
//! When the library is compiled into a WASM module it will include a `main`
//! function which connects the view components to the web page environment.
//!
//! The following HTML elements are looked up by element id:
//! * `catalog-container` (mandatory)
//! * `catalog-config-container` (mandatory)
//! * `export-container` (mandatory)
//! * `map-container` (mandatory)
//! * `track-info-container` (optional)
//! * `map-config-container` (optional)
//! * `version` (optional)
//! * `welcome` (optional)
//!
//! The library can also be used outside the web environment as a native
//! library to process track files. For details take a look at the files in the
//! `examples` folder.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

#![allow(clippy::bool_assert_comparison)]

use wasm_bindgen::prelude::*;

mod controller;
use controller::*;

pub mod edition;

pub mod hexagon;
use hexagon::*;

pub mod import;

#[macro_use]
mod logger;

pub mod map;
mod storage;

#[macro_use]
pub mod tile;

pub mod token;

#[macro_use]
mod view;
use view::*;

//----------------------------------------------------------------------------

#[doc(hidden)]
#[wasm_bindgen]
pub fn main() -> Result<(), JsValue> {
    let window = web_sys::window().unwrap();
    let document = window.document().unwrap();
    let layout = Layout::new(Orientation::pointy(), Point(50.0, 50.0), Point(450.0, 400.0));

    let parent = document.get_element_by_id("catalog-container")
        .ok_or("Cannot find '#catalog-container' parent element for catalog component")?;
    CatalogController::init(CatalogView::new(parent, &layout)?);

    let parent = document.get_element_by_id("map-container")
        .ok_or("Cannot find '#map-container' parent element for map component")?;
    MapController::init(MapView::new(parent, &layout)?);

    let parent = document.get_element_by_id("catalog-config-container")
        .ok_or("Cannot find '#catalog-config-container' parent element for catalog config component")?;
    CatalogConfigController::init(CatalogConfigView::new(parent)?);

    if let Some(parent) = document.get_element_by_id("track-info-container") {
        TrackInfoController::init(TrackInfoView::new(parent)?);
    }

    if let Some(parent) = document.get_element_by_id("map-config-container") {
        MapConfigController::init(MapConfigView::new(parent)?);
    }

    let parent = document.get_element_by_id("export-container")
        .ok_or("Cannot find '#export-container' parent element for export component")?;
    ExportController::init(ExportView::new(parent, &layout)?);

    if let Some(parent) = document.get_element_by_id("version") {
        VersionView::new(parent)?;
    }

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
        nuts::publish(AppendTileEvent { id: tile!(110, b), pos: None, hint: "R".parse().ok() });
        nuts::publish(AppendTileEvent { id: tile!(107, b), pos: None, hint: None });
        nuts::publish(InsertTileEvent { id: tile!(101), pos: (0, -2).into(), dir: Direction::D});
        nuts::publish(AlignCenterEvent);
    }

    // initialize after all other controllers are completed to ignore events during setup
    if let Some(parent) = document.get_element_by_id("welcome") {
        WelcomeController::init(WelcomeView::new(parent)?);
    }

    // restore previous view settings
    nuts::publish(LoadSettingsEvent {});

    Ok(())
}

//----------------------------------------------------------------------------
