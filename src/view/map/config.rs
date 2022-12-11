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
use crate::controller::{UpdateBackgroundGridEvent, UpdateExportScaleEvent, UpdateTileLabelsEvent};
use crate::controller::map_config::*;
use crate::view::export::ExportScale;
use super::*;

//----------------------------------------------------------------------------

struct ExportScaleElement {
    inner: Element,
    value: Option<ExportScale>,
    click_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl ExportScaleElement {
    fn new(inner: Element) -> Result<Self> {
        let value = inner.get_attribute("data-value")
            .and_then(|val| val.parse::<ExportScale>().ok());

        let click_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::send_to::<MapConfigController, _>(UpdateExportScaleEvent { scale: value });
        }) as Box<dyn Fn(_)>);
        inner.add_event_listener_with_callback("click",
            click_cb.as_ref().unchecked_ref())?;

        Ok(ExportScaleElement { inner, value, click_cb })
    }

    fn value(&self) -> Option<ExportScale> {
        self.value
    }

    fn set_active(&self, value: bool) {
        let color_class = match self.value {
            None => "is-dark",
            Some(_) => "is-info",
        };
        if value {
            check!(self.inner.class_list().add_1(color_class).ok());
            check!(self.inner.class_list().add_1("is-selected").ok());
        } else {
            check!(self.inner.class_list().remove_1(color_class).ok());
            check!(self.inner.class_list().remove_1("is-selected").ok());
        }
    }
}

impl AsRef<Element> for ExportScaleElement {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

impl Drop for ExportScaleElement {
    fn drop(&mut self) {
        let _ = self.inner.remove_event_listener_with_callback("click",
            self.click_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------

pub struct MapConfigView {
    inner: Element,
    background_grid: web_sys::HtmlInputElement,
    export_scale: Option<ExportScale>,
    export_scale_elements: Vec<ExportScaleElement>,
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

        let export_scale = None;
        let mut export_scale_elements = Vec::with_capacity(4);
        let node_list = inner.query_selector_all("#export-scale button")?;
        for i in 0..node_list.length() {
            let element = node_list.get(i).unwrap()
                .dyn_into::<web_sys::Element>().unwrap();
                export_scale_elements.push(ExportScaleElement::new(element)?);
        }

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

        Ok(MapConfigView {
            inner, background_grid, export_scale, export_scale_elements,
            tile_labels, apply, apply_cb, close, close_cb
        })
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

    pub fn set_export_scale(&mut self, scale: Option<ExportScale>) {
        for item in &self.export_scale_elements {
            let active = item.value() == scale;
            item.set_active(active);
        }
        self.export_scale = scale;
    }

    pub fn set_tile_labels(&self, visible: bool) {
        self.tile_labels.set_checked(visible);
    }

    pub fn apply_map_config(&mut self) {
        let visible = self.background_grid.checked();
        nuts::publish(UpdateBackgroundGridEvent { visible });
        let scale = self.export_scale;
        nuts::publish(UpdateExportScaleEvent { scale });
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
