//----------------------------------------------------------------------------
//! Controls display of the application version label.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
// $Id$
//----------------------------------------------------------------------------

use log::debug;
use web_sys::{self, Document, Element};

use super::*;

//----------------------------------------------------------------------------

const VERSION: &str = env!("CARGO_PKG_VERSION");

pub struct VersionView {
    inner: Element,
}

impl VersionView {
    pub fn new(document: &Document) -> Result<Self> {
        let inner = document.get_element_by_id("version")
            .ok_or("Cannot find '#version' element")?;

        // remove all pre-existing child nodes
        let range = document.create_range()?;
        range.select_node_contents(&inner)?;
        range.delete_contents()?;

        let mut text = String::with_capacity(10);
        text.push('v');
        text.push_str(VERSION);
        debug!("create version label: {}", text);
        inner.append_child(&document.create_text_node(&text))?;

        Ok(VersionView { inner })
    }
}

//----------------------------------------------------------------------------
