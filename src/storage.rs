//----------------------------------------------------------------------------
//! Storage of view settings.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use serde::ser::Serialize;
use serde::de::DeserializeOwned;

//----------------------------------------------------------------------------

pub struct Storage {
    inner: Option<web_sys::Storage>,
    key: String,
}

impl Storage {
    pub fn new(key: &str) -> Self {
        let inner = web_sys::window()
            .and_then(|wnd| wnd.local_storage().ok().flatten());
        let key = String::from(key);
        Storage { inner, key }
    }

    pub fn with_session_storage(key: &str) -> Self {
        let inner = web_sys::window()
            .and_then(|wnd| wnd.session_storage().ok().flatten());
        let key = String::from(key);
        Storage { inner, key }
    }

    pub fn get<T: DeserializeOwned>(&self) -> Option<T> {
        debug!("get storage key {}", &self.key);
        let value = self.inner.as_ref()
            .and_then(|stor| {
                stor.get_item(&self.key).ok().flatten()
            })
            .and_then(|text| {
                serde_json::from_str::<T>(&text).ok()
            });
        value
    }

    pub fn set<T: Serialize>(&self, value: &T) {
        debug!("set storage key {}", &self.key);
        if let Some(ref stor) = self.inner {
            if let Ok(text) = serde_json::to_string(value) {
                let _ = stor.set_item(&self.key, &text);
            }
        }
    }
}

//----------------------------------------------------------------------------
