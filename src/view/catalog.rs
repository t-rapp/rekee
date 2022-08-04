//----------------------------------------------------------------------------
//! Rendering of the tile catalog and according event handlers.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use std::collections::{BTreeSet, HashMap, HashSet};

use serde::{Serialize, Deserialize};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Document, Element};

use crate::check;
use crate::controller::*;
use crate::edition::{Edition, Series};
use crate::map::Map;
use super::*;

//----------------------------------------------------------------------------

struct CatalogTile {
    inner: Element,
    id: TileId,
    count: usize,
    usage: usize,
    counter: Element,
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
        let tile = PlacedTile::new(id, (0, 0).into(), Direction::A);
        canvas.append_child(&draw_tile_with_label(document, layout, &tile)?.into())?;

        let tile = document.create_element("div")?;
        tile.set_attribute("class", "tile")?;
        tile.set_attribute("draggable", "true")?;
        tile.append_child(&canvas)?;

        let counter = document.create_element("div")?;
        counter.set_attribute("class", "counter tag is-rounded is-light")?;
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

        Ok(CatalogTile { inner: tile, id, count: 0, usage: 0, counter, dblclick_cb, dragstart_cb })
    }

    fn inner_update_counter(&mut self, count: usize, usage: usize) {
        self.count = count;
        self.usage = usage;
        let counter = self.count as i32 - self.usage as i32;
        self.counter.set_inner_html(&counter.to_string());
        if counter <= 0 {
            check!(self.inner.class_list().add_1("is-disabled").ok());
        } else {
            check!(self.inner.class_list().remove_1("is-disabled").ok());
        }
        if counter < 0 {
            check!(self.counter.class_list().remove_1("is-light").ok());
            check!(self.counter.class_list().add_1("is-warning").ok());
        } else {
            check!(self.counter.class_list().add_1("is-light").ok());
            check!(self.counter.class_list().remove_1("is-warning").ok());
        }
    }

    pub fn set_count(&mut self, value: usize) {
        self.inner_update_counter(value, self.usage);
    }

    pub fn set_usage(&mut self, value: usize) {
        self.inner_update_counter(self.count, value);
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
            .and_then(|val| val.parse::<u8>().ok());

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
            .and_then(|val| val.parse::<Terrain>().ok());

        let tiles: BTreeSet<_> = TileInfo::iter()
            .filter(|info| match value {
                Some(val) => info.terrain() == val,
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

#[derive(Serialize, Deserialize)]
#[serde(default)]
pub struct CatalogSettings {
    pub editions: Vec<Edition>,
    pub filter_lanes: Option<u8>,
    pub filter_terrain: Option<Terrain>,
}

impl Default for CatalogSettings {
    fn default() -> Self {
        // list of default editions depends on whether the DIRT feature is enabled or not
        let editions = if cfg!(feature = "dirt") {
            Series::Dirt.editions()
        } else {
            Series::Gt.editions()
        };
        let editions = editions
            .cloned()
            .collect();
        let filter_lanes = None;
        let filter_terrain = None;
        CatalogSettings { editions, filter_lanes, filter_terrain }
    }
}

//----------------------------------------------------------------------------

pub struct CatalogView {
    catalog: Element,
    tiles: Vec<CatalogTile>,
    editions: Vec<Edition>,
    filter_lanes: Option<u8>,
    filter_lanes_elements: Vec<LanesFilterElement>,
    filter_terrain: Option<Terrain>,
    filter_terrain_elements: Vec<TerrainFilterElement>,
    tile_labels_visible: bool,
    map: Option<Element>,
    dragover_cb: Closure<dyn Fn(web_sys::DragEvent)>,
    dragdrop_cb: Closure<dyn Fn(web_sys::DragEvent)>,
    keychange_cb: Closure<dyn Fn(web_sys::KeyboardEvent)>,
    config_button: Element,
    config_show_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl CatalogView {
    pub fn new(parent: Element, layout: &Layout) -> Result<Self> {
        // create layout instance without map offset
        let layout = layout.with_origin(layout.size());

        let document = parent.owner_document().unwrap();

        let catalog = document.create_element("ul")?;
        catalog.set_id("catalog");
        catalog.set_attribute("class", "mt-2")?;

        let editions: Vec<_> = Series::Gt.editions()
            .cloned()
            .collect();

        let mut tile_ids: Vec<TileId> = Edition::iter()
            .flat_map(|edition| edition.tiles())
            .collect();
        tile_ids.sort_unstable();

        let mut tiles = Vec::with_capacity(tile_ids.len());
        let mut base_ids = HashSet::<TileId>::new();
        for tile_id in &tile_ids {
            // create only a single tile for each base identifier, ignore more variants
            if base_ids.contains(&tile_id.base()) {
                continue;
            }
            base_ids.insert(tile_id.base());
            let tile = CatalogTile::new(&document, &layout, *tile_id)?;
            let item = document.create_element("li")?;
            item.append_child(tile.as_ref())?;
            catalog.append_child(&item)?;
            tiles.push(tile);
        }
        parent.append_child(&catalog)?;

        let mut filter_lanes_elements = Vec::with_capacity(4);
        let node_list = parent.query_selector_all("#lanes-filter li")?;
        for i in 0..node_list.length() {
            let element = node_list.get(i).unwrap()
                .dyn_into::<web_sys::Element>().unwrap();
            filter_lanes_elements.push(LanesFilterElement::new(element)?);
        }
        let filter_lanes = None;

        let mut filter_terrain_elements = Vec::with_capacity(4);
        let node_list = parent.query_selector_all("#terrain-filter li")?;
        for i in 0..node_list.length() {
            let element = node_list.get(i).unwrap()
                .dyn_into::<web_sys::Element>().unwrap();
            filter_terrain_elements.push(TerrainFilterElement::new(element)?);
        }
        let filter_terrain = None;

        let tile_labels_visible = true;

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

        let config_button = document.get_element_by_id("catalog-config-button").unwrap();
        let config_show_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::send_to::<CatalogController, _>(SaveSettingsEvent {});
            nuts::publish(ShowCatalogConfigEvent);
        }) as Box<dyn Fn(_)>);
        config_button.add_event_listener_with_callback("click", config_show_cb.as_ref().unchecked_ref()).unwrap();
        config_button.remove_attribute("disabled").unwrap();

        let mut view = CatalogView {
            catalog, tiles, editions, filter_lanes, filter_lanes_elements, filter_terrain,
            filter_terrain_elements, tile_labels_visible, map: None, dragover_cb, dragdrop_cb,
            keychange_cb, config_button, config_show_cb
        };
        view.load_settings(&CatalogSettings::default());
        parent.set_hidden(false);

        Ok(view)
    }

    pub fn load_settings(&mut self, settings: &CatalogSettings) {
        self.inner_update_editions(&settings.editions);
        self.inner_update_filter(settings.filter_lanes, settings.filter_terrain);
    }

    pub fn save_settings(&mut self) -> CatalogSettings {
        let editions = self.editions.clone();
        let filter_lanes = self.filter_lanes;
        let filter_terrain = self.filter_terrain;
        CatalogSettings { editions, filter_lanes, filter_terrain }
    }

    pub fn import_file(&mut self, map: &Map) {
        // ensure that all editions that are used by tiles of the imported map are visible
        let mut used_editions = self.editions.clone();
        for edition in map.tiles().group_by_edition() {
            if !used_editions.contains(&edition) {
                info!("automatically add used edition {}", &edition);
                used_editions.push(edition);
            }
        }
        self.update_editions(&used_editions);
    }

    pub fn update_editions(&mut self, editions: &[Edition]) {
        self.inner_update_editions(editions);
        nuts::send_to::<CatalogController, _>(SaveSettingsEvent {});
    }

    fn inner_update_editions(&mut self, editions: &[Edition]) {
        info!("update editions {:?}", editions);
        self.editions.clear();
        self.editions.extend_from_slice(editions);
        self.editions.sort_unstable();

        // count number of editions per series to determine the active filter types
        let gt_count = self.editions.iter()
            .filter(|val| val.series() == Series::Gt)
            .count();
        let dirt_count = self.editions.iter()
            .filter(|val| val.series() == Series::Dirt)
            .count();
        let lanes_filter_active = gt_count >= dirt_count;
        let terrain_filter_active = dirt_count >= gt_count;

        // show or hide filter types based on the active series, clear filter
        // settings that are not visible and thus can't be changed by the user
        let document = self.catalog.owner_document().unwrap();
        if let Some(element) = document.get_element_by_id("lanes-filter") {
            if !lanes_filter_active {
                self.filter_lanes = None;
            }
            element.set_hidden(!lanes_filter_active);
        } else {
            self.filter_lanes = None;
        }
        if let Some(element) = document.get_element_by_id("terrain-filter") {
            if !terrain_filter_active {
                self.filter_terrain = None;
            }
            element.set_hidden(!terrain_filter_active);
        } else {
            self.filter_terrain = None;
        }

        // calculate the number of available tile variants
        let mut tile_ids: Vec<TileId> = editions.iter()
            .flat_map(|edition| edition.tiles())
            .collect();
        tile_ids.sort_unstable();
        let mut tile_counts = HashMap::<TileId, usize>::new();
        for tile_id in &tile_ids {
            let count = tile_counts.entry(tile_id.base())
                .or_insert(0);
            *count += 1;
        }

        // update the catalog tile elements with number of tile variants
        for tile in self.tiles.iter_mut() {
            let count = tile_counts.get(&tile.id.base())
                .unwrap_or(&0);
            tile.set_count(*count);
        }

        self.inner_update_filter(self.filter_lanes, self.filter_terrain);
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

        let edition_tiles: HashSet<TileId> = self.editions.iter()
            .flat_map(|edition| edition.tiles())
            .map(|id| id.base())
            .collect();

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
            let mut hidden = !edition_tiles.contains(&tile.id.base());
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

    pub fn update_tile_labels(&mut self, visible: bool) {
        if visible != self.tile_labels_visible {
            info!("update catalog tile labels: {:?}", visible);
            self.tile_labels_visible = visible;
            if visible {
                check!(self.catalog.class_list().remove_1("tile-labels-hidden").ok());
            } else {
                check!(self.catalog.class_list().add_1("tile-labels-hidden").ok());
            }
        }
    }

    pub fn toggle_tile_labels(&mut self, inverted: bool) {
        let visible = self.tile_labels_visible ^ inverted;
        info!("toggle catalog tile labels: {:?}", visible);
        if visible {
            check!(self.catalog.class_list().remove_1("tile-labels-hidden").ok());
        } else {
            check!(self.catalog.class_list().add_1("tile-labels-hidden").ok());
        }
    }

    pub fn update_tile_usage(&mut self, tiles: &[TileId]) {
        info!("update tile usage");

        /* when a tile is used, count both sides as used */
        let mut tile_counts = HashMap::<u16, usize>::new();
        for full_id in tiles {
            let count = tile_counts.entry(full_id.num()).or_insert(0);
            *count += 1;
        }
        for tile in self.tiles.iter_mut() {
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
        let _ = self.config_button.remove_event_listener_with_callback("click",
            self.config_show_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------
