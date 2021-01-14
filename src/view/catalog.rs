//----------------------------------------------------------------------------
//! Rendering of the tile catalog and according event handlers.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use std::collections::BTreeSet;

use log::{info};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Document, Element, Storage};

use crate::check;
use crate::controller::*;
use super::*;

//----------------------------------------------------------------------------

struct CatalogTile {
    inner: Element,
    id: TileId,
    dblclick_cb: Closure<dyn Fn(web_sys::MouseEvent)>,
    dragstart_cb: Closure<dyn Fn(web_sys::DragEvent)>,
}

impl CatalogTile {
    fn new(document: &Document, layout: &Layout, id: TileId) -> Result<Self> {
        let canvas = document.create_element_ns(SVG_NS, "svg")?;
        let width = (2.0 * layout.size().x()).round() as i32;
        let height = (2.0 * layout.size().y()).round() as i32;
        canvas.set_attribute("width", &format!("{}px", width))?;
        canvas.set_attribute("height", &format!("{}px", height))?;
        canvas.set_attribute("viewBox", &format!("0 0 {} {}", width, height))?;
        canvas.set_attribute("xmlns", SVG_NS_STR)?;

        let style = document.create_element_ns(SVG_NS, "style")?;
        style.append_child(&document.create_text_node(TILE_STYLE))?;
        canvas.append_child(&style)?;
        canvas.append_child(&draw_tile(&document, &layout, id, (0, 0), Direction::A)?.into())?;

        let tile = document.create_element("div")?;
        tile.set_attribute("class", "tile")?;
        tile.set_attribute("draggable", "true")?;
        tile.append_child(&canvas)?;

        let dblclick_cb = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
            let hint = match (event.shift_key(), event.ctrl_key()) {
                (true, true) => Some(ConnectionHint::Straight),
                (true, false) => Some(ConnectionHint::Left),
                (false, true) => Some(ConnectionHint::Right),
                (false, false) => None,
            };
            nuts::publish(AppendTileEvent { id: id.base(), pos: None, hint });
        }) as Box<dyn Fn(_)>);
        tile.add_event_listener_with_callback("dblclick", dblclick_cb.as_ref().unchecked_ref())?;

        let drag_img = canvas;
        let dragstart_cb = Closure::wrap(Box::new(move |event: web_sys::DragEvent| {
            if let Some(trans) = event.data_transfer() {
                let data = id.base().to_string();
                trans.set_data("application/rekee", &data).unwrap();
                trans.set_data("text/plain", &data).unwrap();
                trans.set_effect_allowed("copy");
                trans.set_drag_image(&drag_img, 50, 50);
            }
            nuts::publish(DragCatalogBeginEvent { tile: id.base() });
        }) as Box<dyn Fn(_)>);
        tile.add_event_listener_with_callback("dragstart", dragstart_cb.as_ref().unchecked_ref())?;

        Ok(CatalogTile { inner: tile, id, dblclick_cb, dragstart_cb })
    }
}

impl AsRef<Element> for CatalogTile {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

impl Drop for CatalogTile {
    fn drop(&mut self) {
        let _ = self.inner.remove_event_listener_with_callback("dblclick",
            self.dblclick_cb.as_ref().unchecked_ref());
        let _ = self.inner.remove_event_listener_with_callback("dragstart",
            self.dragstart_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------

struct FilterItem {
    inner: Element,
    value: Option<u8>,
    tiles: BTreeSet<TileId>,
    anchor: Element,
    click_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl FilterItem {
    fn new(inner: Element) -> Result<Self> {
        let value = inner.get_attribute("data-value")
            .map(|val| val.parse().ok())
            .flatten();

        let tiles: BTreeSet<_> = TileInfo::iter()
            .filter(|info| {
                Direction::iter()
                    .map(|dir| info.edge(*dir).lanes())
                    .any(|val| Some(val) == value)
            })
            .map(|info| info.base_id())
            .collect();

        let click_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::publish(UpdateFilterEvent { lanes: value });
        }) as Box<dyn Fn(_)>);

        let anchor = inner.query_selector("a")?
            .ok_or("Cannot find anchor element of filter item")?;
        anchor.add_event_listener_with_callback("click",
            click_cb.as_ref().unchecked_ref())?;

        Ok(FilterItem { inner, value, tiles, anchor, click_cb })
    }

    fn value(&self) -> Option<u8> {
        self.value
    }

    fn contains(&self, tile: TileId) -> bool {
        match self.value {
            Some(_) => self.tiles.contains(&tile.base()),
            None => true,
        }
    }

    fn set_active(&self, value: bool) {
        if value {
            check!(self.inner.class_list().add_1("is-active").ok());
        } else {
            check!(self.inner.class_list().remove_1("is-active").ok());
        }
    }
}

impl AsRef<Element> for FilterItem {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

impl Drop for FilterItem {
    fn drop(&mut self) {
        let _ = self.anchor.remove_event_listener_with_callback("click",
            self.click_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------

pub struct CatalogView {
    layout: Layout,
    catalog: Element,
    tiles: Vec<CatalogTile>,
    filter_items: Vec<FilterItem>,
    map: Option<Element>,
    storage: Option<Storage>,
    dragover_cb: Closure<dyn Fn(web_sys::DragEvent)>,
    dragdrop_cb: Closure<dyn Fn(web_sys::DragEvent)>,
    keychange_cb: Closure<dyn Fn(web_sys::KeyboardEvent)>,
}

impl CatalogView {
    pub fn new(parent: Element, layout: &Layout) -> Result<Self> {
        // create layout instance without map offset
        let layout = Layout::new(layout.orientation(), layout.size(), layout.size());

        let document = parent.owner_document().unwrap();
        let storage = web_sys::window()
            .and_then(|wnd| wnd.local_storage().ok().flatten());

        let catalog = document.create_element("ul")?;
        catalog.set_id("catalog");
        catalog.set_attribute("class", "mt-2")?;

        let mut tiles = Vec::with_capacity(TileInfo::iter().count());
        for info in TileInfo::iter() {
            let tile = CatalogTile::new(&document, &layout, info.full_id())?;
            let item = document.create_element("li")?;
            item.append_child(tile.as_ref())?;
            catalog.append_child(&item)?;
            tiles.push(tile);
        }
        parent.append_child(&catalog)?;

        let mut filter_items = Vec::with_capacity(4);
        let node_list = parent.query_selector_all("#catalog-filter li")?;
        for i in 0..node_list.length() {
            let element = node_list.get(i).unwrap()
                .dyn_into::<web_sys::Element>().unwrap();
            filter_items.push(FilterItem::new(element)?);
        }

        let dragover_cb = Closure::wrap(Box::new(move |event: web_sys::DragEvent| {
            let data = event.data_transfer()
                .and_then(|trans| trans.get_data("application/rekee").ok());
            if data.is_some() {
                event.prevent_default();
                let pos = check!(mouse_position(&event));
                nuts::publish(DragMapMoveEvent { pos });
            }
        }) as Box<dyn Fn(_)>);

        let dragdrop_cb = Closure::wrap(Box::new(move |event: web_sys::DragEvent| {
            let tile: Option<TileId> = event.data_transfer()
                .and_then(|trans| trans.get_data("application/rekee").ok())
                .and_then(|data| data.parse().ok());
            if let Some(tile) = tile {
                event.prevent_default();
                let pos = check!(mouse_position(&event));
                nuts::publish(DragCatalogEndEvent { pos, tile });
                nuts::publish(UpdateSelectedEvent { pos });
            }
        }) as Box<dyn Fn(_)>);

        let keychange_cb = Closure::wrap(Box::new(move |event: web_sys::KeyboardEvent| {
            if event.repeat() {
                return;
            }
            let hint = match (event.shift_key(), event.ctrl_key()) {
                (true, true) => Some(ConnectionHint::Straight),
                (true, false) => Some(ConnectionHint::Left),
                (false, true) => Some(ConnectionHint::Right),
                (false, false) => None,
            };
            nuts::publish(UpdateConnectionHintEvent { hint });
        }) as Box<dyn Fn(_)>);
        document.add_event_listener_with_callback("keydown",
            keychange_cb.as_ref().unchecked_ref())?;
        document.add_event_listener_with_callback("keyup",
            keychange_cb.as_ref().unchecked_ref())?;

        // restore previous selected filter item
        let mut lanes = None;
        if let Some(ref storage) = storage {
            lanes = storage.get_item("filter")?
                .map(|val| val.parse::<u8>().ok())
                .unwrap_or(lanes);
        }

        let mut view = CatalogView {
            layout, catalog, tiles, filter_items, map: None, storage,
            dragover_cb, dragdrop_cb, keychange_cb
        };
        view.update_filter(lanes);

        Ok(view)
    }

    pub fn update_filter(&mut self, lanes: Option<u8>) {
        info!("update filter lanes: {:?}", lanes);

        let mut filter = None;
        for item in &self.filter_items {
            let active = lanes == item.value();
            item.set_active(active);
            if active {
                filter = Some(item);
            }
        }
        for tile in &self.tiles {
            let hidden = match filter {
                Some(filter) => !filter.contains(tile.id),
                None => false,
            };
            tile.set_hidden(hidden);
        }

        // remember selected filter item
        if let Some(ref storage) = self.storage {
            let value = match lanes {
                Some(val) => val.to_string(),
                None => "all".to_string(),
            };
            let _ = storage.set_item("filter", &value);
        }
    }

    pub fn drag_begin(&mut self, tile: TileId) {
        info!("drag begin: {:?}", tile);
        if self.map.is_none() {
            // lookup map element and initialize drag-n-drop event handlers
            let document = self.catalog.owner_document().unwrap();
            if let Some(map) = document.get_element_by_id("map") {
                check!(map.add_event_listener_with_callback("dragover",
                    self.dragover_cb.as_ref().unchecked_ref()).ok());
                check!(map.add_event_listener_with_callback("drop",
                    self.dragdrop_cb.as_ref().unchecked_ref()).ok());
                self.map = Some(map);
            }
        }
    }
}

impl Drop for CatalogView {
    fn drop(&mut self) {
        let document = self.catalog.owner_document().unwrap();
        let _ = document.remove_event_listener_with_callback("keydown",
            self.keychange_cb.as_ref().unchecked_ref());
        let _ = document.remove_event_listener_with_callback("keyup",
            self.keychange_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------
