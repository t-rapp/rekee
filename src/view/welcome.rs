//----------------------------------------------------------------------------
//! Controls display of the welcome message.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
// $Id$
//----------------------------------------------------------------------------

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Document, Element};

use crate::controller::*;
use super::*;

//----------------------------------------------------------------------------

pub struct WelcomeView {
    inner: Element,
    click_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl WelcomeView {
    pub fn new(document: &Document) -> Result<Self> {
        let inner = document.get_element_by_id("welcome")
            .ok_or("Cannot find '#welcome' element")?;
        inner.set_hidden(false);

        let click_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::publish(HideWelcomeEvent);
        }) as Box<dyn Fn(_)>);

        inner.add_event_listener_with_callback("click",
            click_cb.as_ref().unchecked_ref())?;

        Ok(WelcomeView { inner, click_cb })
    }

    pub fn hide_welcome(&self) {
        self.inner.set_hidden(true);
    }
}

impl Drop for WelcomeView {
    fn drop(&mut self) {
        let _ = self.inner.remove_event_listener_with_callback("click",
            self.click_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------
