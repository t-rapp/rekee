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
use crate::controller::{
    UpdateBackgroundGridEvent, UpdateExportScaleEvent, UpdateExportHeaderEvent,
    UpdateExportListingEvent, UpdateExportColorSchemeEvent, UpdateTileLabelsEvent
};
use crate::controller::map_config::*;
use crate::export::{ExportColorScheme, ExportScale};
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

struct ExportColorSchemeElement {
    inner: Element,
    value: Option<ExportColorScheme>,
    click_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl ExportColorSchemeElement {
    fn new(inner: Element) -> Result<Self> {
        let value = inner.get_attribute("data-value")
            .and_then(|val| val.parse::<ExportColorScheme>().ok());

        let click_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::send_to::<MapConfigController, _>(UpdateExportColorSchemeEvent { color_scheme: value });
        }) as Box<dyn Fn(_)>);
        inner.add_event_listener_with_callback("click",
            click_cb.as_ref().unchecked_ref())?;

        Ok(ExportColorSchemeElement { inner, value, click_cb })
    }

    fn value(&self) -> Option<ExportColorScheme> {
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

impl AsRef<Element> for ExportColorSchemeElement {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

impl Drop for ExportColorSchemeElement {
    fn drop(&mut self) {
        let _ = self.inner.remove_event_listener_with_callback("click",
            self.click_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------

struct LabelTypeElement {
    inner: Element,
    value: LabelType,
    click_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl LabelTypeElement {
    fn new(inner: Element) -> Result<Self> {
        let value = inner.get_attribute("data-value")
            .and_then(|val| val.parse::<LabelType>().ok())
            .unwrap_or_default();

        let click_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::send_to::<MapConfigController, _>(UpdateTileLabelsEvent { label_type: value });
        }) as Box<dyn Fn(_)>);
        inner.add_event_listener_with_callback("click",
            click_cb.as_ref().unchecked_ref())?;

        Ok(LabelTypeElement { inner, value, click_cb })
    }

    fn value(&self) -> LabelType {
        self.value
    }

    fn set_active(&self, value: bool) {
        let color_class = match self.value {
            LabelType::None => "is-dark",
            _ => "is-info",
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

impl AsRef<Element> for LabelTypeElement {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

impl Drop for LabelTypeElement {
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
    export_header: web_sys::HtmlInputElement,
    export_listing: web_sys::HtmlInputElement,
    export_color_scheme: Option<ExportColorScheme>,
    export_color_scheme_elements: Vec<ExportColorSchemeElement>,
    label_type: LabelType,
    label_type_elements: Vec<LabelTypeElement>,
    apply: Element,
    apply_cb: Closure<dyn Fn(web_sys::Event)>,
    close: Element,
    close_cb: Closure<dyn Fn(web_sys::Event)>,
    keydown_cb: Closure<dyn Fn(web_sys::KeyboardEvent)>,
}

impl MapConfigView {
    pub fn new(parent: Element) -> Result<Self> {
        let document = parent.owner_document().unwrap();
        let inner = parent;

        let background_grid = document.get_element_by_id("background-grid")
            .and_then(|elm| elm.dyn_into::<web_sys::HtmlInputElement>().ok())
            .ok_or("Cannot find background grid input of map config element")?;

        let export_scale = None;
        let mut export_scale_elements = Vec::with_capacity(5);
        let node_list = inner.query_selector_all("#export-scale button")?;
        for i in 0..node_list.length() {
            let element = node_list.get(i).unwrap()
                .dyn_into::<web_sys::Element>().unwrap();
            export_scale_elements.push(ExportScaleElement::new(element)?);
        }

        let export_header = document.get_element_by_id("export-header")
            .and_then(|elm| elm.dyn_into::<web_sys::HtmlInputElement>().ok())
            .ok_or("Cannot find export header input of map config element")?;

        let export_listing = document.get_element_by_id("export-listing")
            .and_then(|elm| elm.dyn_into::<web_sys::HtmlInputElement>().ok())
            .ok_or("Cannot find export listing input of map config element")?;

        let export_color_scheme = None;
        let mut export_color_scheme_elements = Vec::with_capacity(3);
        let node_list = inner.query_selector_all("#export-color-scheme button")?;
        for i in 0..node_list.length() {
            let element = node_list.get(i).unwrap()
                .dyn_into::<web_sys::Element>().unwrap();
            export_color_scheme_elements.push(ExportColorSchemeElement::new(element)?);
        }

        let label_type = LabelType::default();
        let mut label_type_elements = Vec::with_capacity(3);
        let node_list = inner.query_selector_all("#label-type button")?;
        for i in 0..node_list.length() {
            let element = node_list.get(i).unwrap()
                .dyn_into::<web_sys::Element>().unwrap();
            label_type_elements.push(LabelTypeElement::new(element)?);
        }

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

        let keydown_cb = Closure::wrap(Box::new(move |event: web_sys::KeyboardEvent| {
            if event.repeat() {
                return;
            }
            if event.key().eq_ignore_ascii_case("enter") && (event.ctrl_key() || event.meta_key()) {
                nuts::send_to::<MapConfigController, _>(ApplyMapConfigEvent);
                nuts::publish(HideMapConfigEvent);
            }
            if event.key().eq_ignore_ascii_case("escape") {
                nuts::publish(HideMapConfigEvent);
            }
        }) as Box<dyn Fn(_)>);

        Ok(MapConfigView {
            inner, background_grid, export_scale, export_scale_elements,
            export_header, export_listing, export_color_scheme,
            export_color_scheme_elements, label_type, label_type_elements,
            apply, apply_cb, close, close_cb, keydown_cb,
        })
    }

    pub fn set_active(&self, value: bool) {
        let document = self.inner.owner_document().unwrap();

        if value {
            check!(self.inner.class_list().add_1("is-active").ok());
            check!(document.add_event_listener_with_callback("keydown", self.keydown_cb.as_ref().unchecked_ref()).ok());
        } else {
            check!(self.inner.class_list().remove_1("is-active").ok());
            check!(document.remove_event_listener_with_callback("keydown", self.keydown_cb.as_ref().unchecked_ref()).ok());
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

    pub fn set_export_header(&self, visible: bool) {
        self.export_header.set_checked(visible);
    }

    pub fn set_export_listing(&self, visible: bool) {
        self.export_listing.set_checked(visible);
    }

    pub fn set_export_color_scheme(&mut self, color_scheme: Option<ExportColorScheme>) {
        for item in &self.export_color_scheme_elements {
            let active = item.value() == color_scheme;
            item.set_active(active);
        }
        self.export_color_scheme = color_scheme;
    }

    pub fn set_label_type(&mut self, label_type: LabelType) {
        for item in &self.label_type_elements {
            let active = item.value() == label_type;
            item.set_active(active);
        }
        self.label_type = label_type;
    }

    pub fn apply_map_config(&mut self) {
        let visible = self.background_grid.checked();
        nuts::publish(UpdateBackgroundGridEvent { visible });
        let scale = self.export_scale;
        nuts::publish(UpdateExportScaleEvent { scale });
        let visible = self.export_header.checked();
        nuts::publish(UpdateExportHeaderEvent { visible });
        let visible = self.export_listing.checked();
        nuts::publish(UpdateExportListingEvent { visible });
        let color_scheme = self.export_color_scheme;
        nuts::publish(UpdateExportColorSchemeEvent { color_scheme });
        let label_type = self.label_type;
        nuts::publish(UpdateTileLabelsEvent { label_type });
    }
}

impl Drop for MapConfigView {
    fn drop(&mut self) {
        let document = self.inner.owner_document().unwrap();

        let _ = self.apply.remove_event_listener_with_callback("click",
            self.apply_cb.as_ref().unchecked_ref());
        let _ = self.close.remove_event_listener_with_callback("click",
            self.close_cb.as_ref().unchecked_ref());
        let _ = document.remove_event_listener_with_callback("keydown",
            self.keydown_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------
