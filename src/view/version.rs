//----------------------------------------------------------------------------
//! Controls display of the application version label.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use web_sys::{self, Element};

use super::*;

//----------------------------------------------------------------------------

const VERSION: &str = env!("CARGO_PKG_VERSION");

pub struct VersionView {
    inner: Element,
}

impl VersionView {
    pub fn new(parent: Element) -> Result<Self> {
        let document = parent.owner_document().unwrap();
        let inner = parent;

        // remove all pre-existing child nodes
        let range = document.create_range()?;
        range.select_node_contents(&inner)?;
        range.delete_contents()?;

        let mut text = String::with_capacity(10);
        text.push('v');
        text.push_str(VERSION);
        debug!("create version label: {}", &text);
        inner.append_child(&document.create_text_node(&text))?;

        Ok(VersionView { inner })
    }
}

impl AsRef<Element> for VersionView {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

//----------------------------------------------------------------------------
