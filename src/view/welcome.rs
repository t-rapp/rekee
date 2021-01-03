//----------------------------------------------------------------------------
//! Controls display of the welcome message.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
// $Id$
//----------------------------------------------------------------------------

use log::debug;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Document, Element, Storage};

use crate::controller::*;
use super::*;

//----------------------------------------------------------------------------

pub struct WelcomeView {
    inner: Element,
    storage: Option<Storage>,
    click_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl WelcomeView {
    pub fn new(document: &Document) -> Result<Self> {
        let inner = document.get_element_by_id("welcome")
            .ok_or("Cannot find '#welcome' element")?;

        let storage = web_sys::window()
            .and_then(|wnd| wnd.session_storage().ok().flatten());

        // restore previous visibility state
        let mut hidden = false;
        if let Some(ref storage) = storage {
            hidden = storage.get_item("welcome")?
                .and_then(|val| val.parse::<bool>().ok())
                .unwrap_or(hidden);
        }
        debug!("create welcome hidden: {}", hidden);
        inner.set_hidden(hidden);

        let click_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::publish(HideWelcomeEvent);
        }) as Box<dyn Fn(_)>);

        inner.add_event_listener_with_callback("click",
            click_cb.as_ref().unchecked_ref())?;

        Ok(WelcomeView { inner, storage, click_cb })
    }

    pub fn set_hidden(&self, value: bool) {
        debug!("update welcome hidden: {}", value);
        self.inner.set_hidden(value);

        // remember visibility state
        if let Some(ref storage) = self.storage {
            let _ = storage.set_item("welcome", &value.to_string());
        }
    }
}

impl Drop for WelcomeView {
    fn drop(&mut self) {
        let _ = self.inner.remove_event_listener_with_callback("click",
            self.click_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------
