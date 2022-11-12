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

use crate::controller::*;
use crate::controller::welcome::*;
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
    click_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl WelcomeView {
    pub fn new(parent: Element) -> Result<Self> {
        let inner = parent;

        let header = inner.query_selector(".message-header")?
            .ok_or("Cannot find header of welcome message element")?;
        inner.set_hidden(false);

        let click_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::publish(HideWelcomeEvent);
        }) as Box<dyn Fn(_)>);

        header.add_event_listener_with_callback("click",
            click_cb.as_ref().unchecked_ref())?;
        header.class_list().add_1("is-clickable")?;

        Ok(WelcomeView { inner, header, click_cb })
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
        nuts::send_to::<WelcomeController, _>(SaveSettingsEvent {});
    }
}

impl Drop for WelcomeView {
    fn drop(&mut self) {
        let _ = self.header.remove_event_listener_with_callback("click",
            self.click_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------
