//----------------------------------------------------------------------------
//! Rendering of the tile catalog and according event handlers.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use std::collections::{BTreeSet, HashMap};

use serde::{Serialize, Deserialize};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Document, Element};

use crate::check;
use crate::controller::*;
use crate::edition::Edition;
use super::*;

//----------------------------------------------------------------------------

struct CatalogTile {
    inner: Element,
    id: TileId,
    count: usize,
    counter: Element,
    dblclick_cb: Closure<dyn Fn(web_sys::MouseEvent)>,
    dragstart_cb: Closure<dyn Fn(web_sys::DragEvent)>,
}

impl CatalogTile {
    fn new(document: &Document, layout: &Layout, id: TileId, count: usize) -> Result<Self> {
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

        let counter = document.create_element("div")?;
        counter.set_attribute("class", "counter tag is-rounded is-light")?;
        let text = count.to_string();
        counter.append_child(&document.create_text_node(&text))?;
        tile.append_child(&counter)?;

        let dblclick_cb = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
            let hint = match (event.shift_key(), event.ctrl_key() || event.meta_key()) {
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

        Ok(CatalogTile { inner: tile, id, count, counter, dblclick_cb, dragstart_cb })
    }

    fn set_usage(&self, value: usize) {
        let count = self.count as i32 - value as i32;
        let text = count.to_string();
        self.counter.set_inner_html(&text);
        if count <= 0 {
            check!(self.inner.class_list().add_1("is-disabled").ok());
        } else {
            check!(self.inner.class_list().remove_1("is-disabled").ok());
        }
        if count < 0 {
            check!(self.counter.class_list().remove_1("is-light").ok());
            check!(self.counter.class_list().add_1("is-warning").ok());
        } else {
            check!(self.counter.class_list().add_1("is-light").ok());
            check!(self.counter.class_list().remove_1("is-warning").ok());
        }
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

struct LanesFilterElement {
    inner: Element,
    value: Option<u8>,
    tiles: BTreeSet<TileId>,
    anchor: Element,
    click_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl LanesFilterElement {
    fn new(inner: Element) -> Result<Self> {
        let value = inner.get_attribute("data-value")
            .map(|val| val.parse::<u8>().ok())
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
            nuts::publish(UpdateLanesFilterEvent { lanes: value });
        }) as Box<dyn Fn(_)>);

        let anchor = inner.query_selector("a")?
            .ok_or("Cannot find anchor element of filter item")?;
        anchor.add_event_listener_with_callback("click",
            click_cb.as_ref().unchecked_ref())?;

        Ok(LanesFilterElement { inner, value, tiles, anchor, click_cb })
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

impl AsRef<Element> for LanesFilterElement {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

impl Drop for LanesFilterElement {
    fn drop(&mut self) {
        let _ = self.anchor.remove_event_listener_with_callback("click",
            self.click_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------

struct TerrainFilterElement {
    inner: Element,
    value: Option<Terrain>,
    tiles: BTreeSet<TileId>,
    anchor: Element,
    click_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl TerrainFilterElement {
    fn new(inner: Element) -> Result<Self> {
        let value = inner.get_attribute("data-value")
            .map(|val| val.parse::<Terrain>().ok())
            .flatten();

        let tiles: BTreeSet<_> = TileInfo::iter()
            .filter(|info| match value {
                Some(val) => info.terrain().eq_surface(val),
                None => true,
            })
            .map(|info| info.base_id())
            .collect();

        let click_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::publish(UpdateTerrainFilterEvent { terrain: value });
        }) as Box<dyn Fn(_)>);

        let anchor = inner.query_selector("a")?
            .ok_or("Cannot find anchor element of filter item")?;
        anchor.add_event_listener_with_callback("click",
            click_cb.as_ref().unchecked_ref())?;

        Ok(TerrainFilterElement { inner, value, tiles, anchor, click_cb })
    }

    fn value(&self) -> Option<Terrain> {
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

impl AsRef<Element> for TerrainFilterElement {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

impl Drop for TerrainFilterElement {
    fn drop(&mut self) {
        let _ = self.anchor.remove_event_listener_with_callback("click",
            self.click_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------

#[derive(Default, Serialize, Deserialize)]
pub struct CatalogSettings {
    #[serde(default)]
    filter_lanes: Option<u8>,
    #[serde(default)]
    filter_terrain: Option<Terrain>,
}

//----------------------------------------------------------------------------

pub struct CatalogView {
    catalog: Element,
    tiles: Vec<CatalogTile>,
    filter_lanes: Option<u8>,
    filter_lanes_elements: Vec<LanesFilterElement>,
    filter_terrain: Option<Terrain>,
    filter_terrain_elements: Vec<TerrainFilterElement>,
    map: Option<Element>,
    dragover_cb: Closure<dyn Fn(web_sys::DragEvent)>,
    dragdrop_cb: Closure<dyn Fn(web_sys::DragEvent)>,
    keychange_cb: Closure<dyn Fn(web_sys::KeyboardEvent)>,
}

impl CatalogView {
    pub fn new(parent: Element, layout: &Layout) -> Result<Self> {
        // create layout instance without map offset
        let layout = Layout::new(layout.orientation(), layout.size(), layout.size());

        let document = parent.owner_document().unwrap();

        let catalog = document.create_element("ul")?;
        catalog.set_id("catalog");
        catalog.set_attribute("class", "mt-2")?;

        // TODO: use both editions here once the user interface is ready
        let is_dirt = cfg!(feature = "dirt");
        let tile_ids = if is_dirt {
            Edition::dirt_tiles()
        } else {
            Edition::gt_tiles()
        };
        let mut tile_counts = HashMap::<TileId, usize>::new();
        for full_id in &tile_ids {
            let count = tile_counts.entry(full_id.base()).or_insert(0);
            *count += 1;
        }

        let mut tiles = Vec::with_capacity(tile_ids.len());
        for full_id in &tile_ids {
            /* create only a single tile for each base identifier, ignore more variants */
            let count = match tile_counts.remove(&full_id.base()) {
                Some(val) => val,
                None => continue,
            };
            let tile = CatalogTile::new(&document, &layout, *full_id, count)?;
            let item = document.create_element("li")?;
            item.append_child(tile.as_ref())?;
            catalog.append_child(&item)?;
            tiles.push(tile);
        }
        parent.append_child(&catalog)?;

        if let Some(element) = document.get_element_by_id("lanes-filter") {
            element.set_hidden(is_dirt);
        }
        let mut filter_lanes_elements = Vec::with_capacity(4);
        if !is_dirt {
            let node_list = parent.query_selector_all("#lanes-filter li")?;
            for i in 0..node_list.length() {
                let element = node_list.get(i).unwrap()
                    .dyn_into::<web_sys::Element>().unwrap();
                filter_lanes_elements.push(LanesFilterElement::new(element)?);
            }
        }
        let filter_lanes = None;

        if let Some(element) = document.get_element_by_id("terrain-filter") {
            element.set_hidden(!is_dirt);
        }
        let mut filter_terrain_elements = Vec::with_capacity(4);
        if is_dirt {
            let node_list = parent.query_selector_all("#terrain-filter li")?;
            for i in 0..node_list.length() {
                let element = node_list.get(i).unwrap()
                    .dyn_into::<web_sys::Element>().unwrap();
                filter_terrain_elements.push(TerrainFilterElement::new(element)?);
            }
        }
        let filter_terrain = None;

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
            let hint = match (event.shift_key(), event.ctrl_key() || event.meta_key()) {
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

        Ok(CatalogView {
            catalog, tiles, filter_lanes, filter_lanes_elements, filter_terrain,
            filter_terrain_elements, map: None, dragover_cb, dragdrop_cb, keychange_cb
        })
    }

    pub fn load_settings(&mut self, settings: &CatalogSettings) {
        self.inner_update_filter(settings.filter_lanes, settings.filter_terrain);
    }

    pub fn save_settings(&mut self) -> CatalogSettings {
        let filter_lanes = self.filter_lanes;
        let filter_terrain = self.filter_terrain;
        CatalogSettings { filter_lanes, filter_terrain }
    }

    pub fn update_lanes_filter(&mut self, lanes: Option<u8>) {
        self.inner_update_filter(lanes, self.filter_terrain);
        nuts::send_to::<CatalogController, _>(SaveSettingsEvent {});
    }

    pub fn update_terrain_filter(&mut self, terrain: Option<Terrain>) {
        self.inner_update_filter(self.filter_lanes, terrain);
        nuts::send_to::<CatalogController, _>(SaveSettingsEvent {});
    }

    fn inner_update_filter(&mut self, lanes: Option<u8>, terrain: Option<Terrain>) {
        info!("update filter: lanes={:?}, terrain={:?}", lanes, terrain);
        let terrain = terrain.map(|val| val.surface());

        let mut lanes_element = None;
        for item in &self.filter_lanes_elements {
            let active = item.value() == lanes;
            item.set_active(active);
            if active {
                lanes_element = Some(item);
            }
        }

        let mut terrain_element = None;
        for item in &self.filter_terrain_elements {
            let active = item.value() == terrain;
            item.set_active(active);
            if active {
                terrain_element = Some(item);
            }
        }

        for tile in &self.tiles {
            let mut hidden = false;
            if let Some(filter) = lanes_element {
                hidden |= !filter.contains(tile.id);
            }
            if let Some(filter) = terrain_element {
                hidden |= !filter.contains(tile.id);
            }
            tile.set_hidden(hidden);
        }

        self.filter_lanes = lanes;
        self.filter_terrain = terrain;
    }

    pub fn update_tile_usage(&mut self, tiles: &[TileId]) {
        info!("update tile usage");

        /* when a tile is used, count both sides as used */
        let mut tile_counts = HashMap::<u16, usize>::new();
        for full_id in tiles {
            let count = tile_counts.entry(full_id.num()).or_insert(0);
            *count += 1;
        }
        for tile in &self.tiles {
            let count = tile_counts.get(&tile.id.num()).unwrap_or(&0);
            tile.set_usage(*count);
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
