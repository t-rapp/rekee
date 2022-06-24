//----------------------------------------------------------------------------
//! Configuration of general track settings used for display and export.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Element};

use crate::check;
use crate::controller::*;
use super::*;

//----------------------------------------------------------------------------

pub struct MapConfigView {
    inner: Element,
    background_grid: web_sys::HtmlInputElement,
    tile_labels: web_sys::HtmlInputElement,
    apply: Element,
    apply_cb: Closure<dyn Fn(web_sys::Event)>,
    close: Element,
    close_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl MapConfigView {
    pub fn new(parent: Element) -> Result<Self> {
        let document = parent.owner_document().unwrap();
        let inner = parent;

        let background_grid = document.get_element_by_id("background-grid")
            .and_then(|elm| elm.dyn_into::<web_sys::HtmlInputElement>().ok())
            .ok_or("Cannot find background grid input of map config element")?;

        let tile_labels = document.get_element_by_id("tile-labels")
            .and_then(|elm| elm.dyn_into::<web_sys::HtmlInputElement>().ok())
            .ok_or("Cannot find tile labels input of map config element")?;

        let apply = document.get_element_by_id("apply-map-config")
            .ok_or("Cannot find apply button of map config element")?;

        let apply_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::send_to::<MapConfigController, _>(ApplyMapConfigEvent);
            nuts::publish(HideMapConfigEvent);
        }) as Box<dyn Fn(_)>);
        apply.add_event_listener_with_callback("click", apply_cb.as_ref().unchecked_ref())?;

        let close_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::publish(HideMapConfigEvent);
        }) as Box<dyn Fn(_)>);

        let close = inner.query_selector(".modal-close")?
            .ok_or("Cannot find close button of map config element")?;
        close.add_event_listener_with_callback("click", close_cb.as_ref().unchecked_ref())?;

        Ok(MapConfigView { inner, background_grid, tile_labels, apply, apply_cb, close, close_cb })
    }

    pub fn set_active(&self, value: bool) {
        if value {
            check!(self.inner.class_list().add_1("is-active").ok());
        } else {
            check!(self.inner.class_list().remove_1("is-active").ok());
        }
    }

    pub fn set_background_grid(&self, visible: bool) {
        self.background_grid.set_checked(visible);
    }

    pub fn set_tile_labels(&self, visible: bool) {
        self.tile_labels.set_checked(visible);
    }

    pub fn apply_map_config(&mut self) {
        let visible = self.background_grid.checked();
        nuts::publish(UpdateBackgroundGridEvent { visible });
        let visible = self.tile_labels.checked();
        nuts::publish(UpdateTileLabelsEvent { visible });
    }
}

impl Drop for MapConfigView {
    fn drop(&mut self) {
        let _ = self.apply.remove_event_listener_with_callback("click",
            self.apply_cb.as_ref().unchecked_ref());
        let _ = self.close.remove_event_listener_with_callback("click",
            self.close_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------
