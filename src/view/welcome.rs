//----------------------------------------------------------------------------
//! Controls display of the welcome message.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use serde::{Serialize, Deserialize};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Element};

use crate::check;
use crate::controller::{SaveSettingsEvent, UpdateCatalogEditionsEvent};
use crate::controller::welcome::*;
use crate::edition::{Edition, Series};
use super::*;

//----------------------------------------------------------------------------

#[derive(Default, Serialize, Deserialize)]
#[serde(default)]
pub struct WelcomeSettings {
    pub hidden: bool,
}

//----------------------------------------------------------------------------

pub struct WelcomeView {
    inner: Element,
    header: Element,
    close_cb: Closure<dyn Fn(web_sys::Event)>,
    catalog_gt_button: Option<Element>,
    catalog_dirt_button: Option<Element>,
    catalog_all_button: Option<Element>,
    select_catalog_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl WelcomeView {
    pub fn new(parent: Element) -> Result<Self> {
        let document = parent.owner_document().unwrap();
        let inner = parent;

        let header = inner.query_selector(".message-header")?
            .ok_or("Cannot find header of welcome message element")?;
        inner.set_hidden(false);

        let close_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::publish(HideWelcomeEvent);
        }) as Box<dyn Fn(_)>);

        header.add_event_listener_with_callback("click",
            close_cb.as_ref().unchecked_ref())?;
        header.class_list().add_1("is-clickable")?;

        let select_catalog_cb = Closure::wrap(Box::new(move |event: web_sys::Event| {
            let button = check!(event.current_target()
                .and_then(|target| target.dyn_into::<web_sys::HtmlElement>().ok()));
            let series: Option<Series> = button.get_attribute("data-value")
                .and_then(|val| val.parse().ok());
            let edition_iter = if let Some(series) = series {
                series.editions()
            } else {
                Edition::iter()
            };
            let editions: Vec<Edition> = edition_iter.copied()
                    .collect();
            nuts::publish(UpdateCatalogEditionsEvent { editions });
        }) as Box<dyn Fn(_)>);

        let catalog_gt_button = document.get_element_by_id("select-catalog-gt-button");
        if let Some(ref button) = catalog_gt_button {
            button.add_event_listener_with_callback("click", select_catalog_cb.as_ref().unchecked_ref())?;
        }

        let catalog_dirt_button = document.get_element_by_id("select-catalog-dirt-button");
        if let Some(ref button) = catalog_dirt_button {
            button.add_event_listener_with_callback("click", select_catalog_cb.as_ref().unchecked_ref())?;
        }

        let catalog_all_button = document.get_element_by_id("select-catalog-all-button");
        if let Some(ref button) = catalog_all_button {
            button.add_event_listener_with_callback("click", select_catalog_cb.as_ref().unchecked_ref())?;
        }

        Ok(WelcomeView {
            inner, header, close_cb, catalog_gt_button, catalog_dirt_button,
            catalog_all_button, select_catalog_cb
        })
    }

    pub fn load_settings(&mut self, settings: &WelcomeSettings) {
        self.inner.set_hidden(settings.hidden);
    }

    pub fn save_settings(&mut self) -> WelcomeSettings {
        let hidden = self.inner.hidden();
        WelcomeSettings { hidden }
    }

    pub fn set_hidden(&self, value: bool) {
        debug!("update welcome hidden: {}", value);
        self.inner.set_hidden(value);

        // remember visibility state
        nuts::send_to::<WelcomeController, _>(SaveSettingsEvent);
    }
}

impl Drop for WelcomeView {
    fn drop(&mut self) {
        let _ = self.header.remove_event_listener_with_callback("click",
            self.close_cb.as_ref().unchecked_ref());
        if let Some(ref button) = self.catalog_gt_button {
            let _ = button.remove_event_listener_with_callback("click",
                self.select_catalog_cb.as_ref().unchecked_ref());
        }
        if let Some(ref button) = self.catalog_dirt_button {
            let _ = button.remove_event_listener_with_callback("click",
                self.select_catalog_cb.as_ref().unchecked_ref());
        }
        if let Some(ref button) = self.catalog_all_button {
            let _ = button.remove_event_listener_with_callback("click",
                self.select_catalog_cb.as_ref().unchecked_ref());
        }
    }
}

//----------------------------------------------------------------------------
