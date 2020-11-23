//----------------------------------------------------------------------------
//! Rendering of the map and according event handlers.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//
// $Id$
//----------------------------------------------------------------------------

use log::{warn, info, debug};
use js_sys;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Document, Element};

use crate::check;
use crate::controller::*;
use crate::import;
use super::*;

//----------------------------------------------------------------------------

fn define_grid_hex(document: &Document, layout: &Layout) -> Result<Element>
{
    let corners = layout.hexagon_corners((0, 0).into());
    let points: Vec<String> = corners.iter()
        .map(|p| *p - layout.origin())
        .map(|p| format!("{},{}", p.x(), p.y()))
        .collect();
    let hex = document.create_element_ns(SVG_NS, "polygon")?;
    hex.set_id("hex");
    hex.set_attribute("points", &points.join(" "))?;
    Ok(hex)
}

fn use_grid_hex<C>(document: &Document, layout: &Layout, pos: C) -> Result<Element>
    where C: Into<Coordinate>
{
    let pos = pos.into().to_pixel(&layout);
    let hex = document.create_element_ns(SVG_NS, "use")?;
    hex.set_attribute("href", "#hex")?;
    hex.set_attribute("x", &pos.x().to_string())?;
    hex.set_attribute("y", &pos.y().to_string())?;
    Ok(hex)
}

fn draw_label<C>(document: &Document, layout: &Layout, pos: C, text: &str) -> Result<Element>
    where C: Into<Coordinate>
{
    let pos = pos.into().to_pixel(&layout);
    let label = document.create_element_ns(SVG_NS, "text")?;
    label.set_attribute("class", "label")?;
    label.set_attribute("x", &pos.x().to_string())?;
    label.set_attribute("y", &pos.y().to_string())?;
    label.append_child(&document.create_text_node(text))?;
    Ok(label)
}

//----------------------------------------------------------------------------

struct SelectedHex {
    inner: Element,
    pos: Option<Coordinate>,
}

impl SelectedHex {
    fn new(document: &Document, layout: &Layout) -> Result<Self> {
        let corners = layout.hexagon_corners((0, 0).into());
        let points: Vec<String> = corners.iter()
            .map(|p| *p - layout.origin())
            .map(|p| format!("{},{}", p.x(), p.y()))
            .collect();
        let poly = document.create_element_ns(SVG_NS, "polygon")?;
        poly.set_attribute("points", &points.join(" "))?;

        let hex = document.create_element_ns(SVG_NS, "g")?;
        hex.set_id("selected");
        hex.set_hidden(true);
        hex.append_child(&poly)?;

        Ok(SelectedHex { inner: hex, pos: None })
    }

    fn pos(&self) -> Option<Coordinate> {
        self.pos
    }

    fn set_pos(&mut self, layout: &Layout, pos: Option<Coordinate>) {
        if pos.is_some() && pos != self.pos {
            let pos = pos.unwrap().to_pixel(&layout);
            let transform = format!("translate({:.3} {:.3})", pos.x(), pos.y());
            check!(self.inner.set_attribute("transform", &transform).ok());
        }
        self.set_hidden(!pos.is_some());
        self.pos = pos;
    }

    fn set_draggable(&self, value: bool) {
        if value {
            check!(self.inner.class_list().add_1("is-draggable").ok());
        } else {
            check!(self.inner.class_list().remove_1("is-draggable").ok());
        }
    }
}

impl AsRef<Element> for SelectedHex {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

//----------------------------------------------------------------------------

struct SelectedMenu {
    inner: Element,
}

impl SelectedMenu {
    fn new(document: &Document, layout: &Layout, pos: Coordinate) -> Result<Self> {
        let pos = pos.to_pixel(&layout);
        let menu = document.create_element_ns(SVG_NS, "foreignObject")?;
        menu.set_attribute("x", &(pos.x() - 60.0).to_string())?;
        menu.set_attribute("y", &(pos.y() + layout.size().y() - 8.0).to_string())?;
        menu.set_attribute("width", "120")?;
        menu.set_attribute("height", "40")?;

        if let Some(btn) = document.get_element_by_id("rotate-selected-left-button") {
            let callback = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
                event.prevent_default();
                event.stop_propagation();
                nuts::publish(RotateSelectedLeftEvent);
            }) as Box<dyn Fn(_)>);
            btn.add_event_listener_with_callback("mousedown", callback.as_ref().unchecked_ref()).unwrap();
            callback.forget();
        }

        if let Some(btn) = document.get_element_by_id("rotate-selected-right-button") {
            let callback = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
                event.prevent_default();
                event.stop_propagation();
                nuts::publish(RotateSelectedRightEvent);
            }) as Box<dyn Fn(_)>);
            btn.add_event_listener_with_callback("mousedown", callback.as_ref().unchecked_ref()).unwrap();
            callback.forget();
        }

        if let Some(btn) = document.get_element_by_id("remove-selected-button") {
            let callback = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
                event.prevent_default();
                event.stop_propagation();
                nuts::publish(RemoveSelectedEvent);
            }) as Box<dyn Fn(_)>);
            btn.add_event_listener_with_callback("mousedown", callback.as_ref().unchecked_ref()).unwrap();
            callback.forget();
        }

        // extract menu element from document after all buttons have been assigned,
        // otherwise get_element_by_id() would not have been able to find the buttons
        let inner = document.get_element_by_id("selected-menu")
            .ok_or_else(|| "Cannot find '#selected-menu' element within page")?;
        menu.append_child(&inner)?;

        Ok(SelectedMenu { inner: menu })
    }

    fn set_pos(&self, layout: &Layout, pos: Coordinate) {
        let pos = pos.to_pixel(&layout);
        check!(self.inner.set_attribute("x", &(pos.x() - 60.0).to_string()).ok());
        check!(self.inner.set_attribute("y", &(pos.y() + layout.size().y() - 8.0).to_string()).ok());
    }
}

impl AsRef<Element> for SelectedMenu {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

//----------------------------------------------------------------------------

struct ActiveHex {
    inner: Element,
    pos: Coordinate,
    dir: Direction,
}

impl ActiveHex {
    fn new(document: &Document, layout: &Layout) -> Result<Self> {
        let pos = Coordinate::new(0, 0);
        let dir = Direction::A;

        let img = document.create_element_ns(SVG_NS, "image")?;
        img.set_attribute("href", "./pkg/arrow-down-circle.svg")?;
        let size = Point(16.0, 16.0);
        img.set_attribute("width", &format!("{}", 2.0 * size.x()))?;
        img.set_attribute("height", &format!("{}", 2.0 * size.y()))?;
        img.set_attribute("transform", &format!("translate({:.3} {:.3})", -size.x(), -size.y()))?;

        let inner = document.create_element_ns(SVG_NS, "g")?;
        inner.set_id("active");
        let inner_pos = pos.to_pixel(&layout);
        let angle = dir.to_angle(&layout);
        let transform = format!("translate({:.3} {:.3}) rotate({:.0})", inner_pos.x(), inner_pos.y(), angle);
        inner.set_attribute("transform", &transform)?;
        inner.append_child(&img)?;

        Ok(ActiveHex { inner, pos, dir })
    }

    fn update(&mut self, layout: &Layout, pos: Coordinate, dir: Direction) {
        if pos != self.pos || dir != self.dir {
            let pos = pos.to_pixel(&layout);
            let angle = dir.to_angle(&layout);
            let transform = format!("translate({:.3} {:.3}) rotate({:.0})", pos.x(), pos.y(), angle);
            check!(self.inner.set_attribute("transform", &transform).ok());
        }
        self.pos = pos;
        self.dir = dir;
    }
}

impl AsRef<Element> for ActiveHex {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

//----------------------------------------------------------------------------

struct DraggedTile {
    inner: Element,
    tile: PlacedTile,
}

impl DraggedTile {
    fn new(document: &Document, layout: &Layout, tile: PlacedTile) -> Result<Self> {
        let size = layout.size();
        let angle = tile.dir.to_angle(&layout);
        let img = document.create_element_ns(SVG_NS, "image")?;
        img.set_attribute("href", &format!("img/thumb-{}.png", tile.id))?;
        img.set_attribute("width", &format!("{}", 2.0 * size.x()))?;
        img.set_attribute("height", &format!("{}", 2.0 * size.y()))?;
        img.set_attribute("transform", &format!("rotate({:.0}) translate({:.3} {:.3})", angle, -size.x(), -size.y()))?;

        let pos = tile.pos.to_pixel(&layout);
        let inner = document.create_element_ns(SVG_NS, "g")?;
        inner.set_id("dragged");
        inner.set_attribute("class", "tile")?;
        inner.set_attribute("transform", &format!("translate({:.3} {:.3})", pos.x(), pos.y()))?;
        inner.append_child(&img)?;

        Ok(DraggedTile{ inner, tile })
    }

    fn tile(&self) -> &PlacedTile {
        &self.tile
    }

    fn set_pos(&self, pos: Point) {
        check!(self.inner.set_attribute("transform",
            &format!("translate({:.3} {:.3})", pos.x(), pos.y())).ok());
    }
}

impl AsRef<Element> for DraggedTile {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

//----------------------------------------------------------------------------

pub struct MapView {
    layout: Layout,
    map: Map,
    canvas: Element,
    tiles: Element,
    selected: SelectedHex,
    selected_menu: SelectedMenu,
    active: ActiveHex,
    dragged: Option<DraggedTile>,
    dragged_mousemove_cb: Closure<dyn Fn(web_sys::MouseEvent)>,
    dragged_mouseup_cb: Closure<dyn Fn(web_sys::MouseEvent)>,
    dragged_mouseleave_cb: Closure<dyn Fn(web_sys::MouseEvent)>,
}

impl MapView {
    pub fn new(parent: Element, layout: Layout) -> Result<Self> {
        let map = Map::new();
        let document = parent.owner_document().unwrap();

        // remove all pre-existing child nodes
        let range = document.create_range()?;
        range.select_node_contents(&parent)?;
        range.delete_contents()?;

        let canvas = document.create_element_ns(SVG_NS, "svg")?;
        canvas.set_id("map");
        let width = (2.0 * layout.origin().x()).round() as i32;
        canvas.set_attribute("width", &format!("{}px", width))?;
        let height = (2.0 * layout.origin().y()).round() as i32;
        canvas.set_attribute("height", &format!("{}px", height))?;
        canvas.set_attribute("viewBox", &format!("0 0 {} {}", width, height))?;
        canvas.set_attribute("xmlns", SVG_NS_STR)?;
        parent.append_child(&canvas)?;

        let defs = document.create_element_ns(SVG_NS, "defs")?;
        defs.append_child(&define_grid_hex(&document, &layout)?.into())?;
        canvas.append_child(&defs)?;

        let style = document.create_element_ns(SVG_NS, "style")?;
        style.append_child(&document.create_text_node(TILE_STYLE))?;
        canvas.append_child(&style)?;

        let group = document.create_element_ns(SVG_NS, "g")?;
        group.set_id("grid");
        let map_radius = 4;
        for q in -map_radius..=map_radius {
            let r1 = i32::max(-map_radius, -q - map_radius);
            let r2 = i32::min(map_radius, -q + map_radius);
            for r in r1..=r2 {
                group.append_child(&use_grid_hex(&document, &layout, (q, r))?.into())?;
            }
        }
        group.append_child(&draw_label(&document, &layout, (map_radius, 0), "+q")?.into())?;
        group.append_child(&draw_label(&document, &layout, (-map_radius, 0), "-q")?.into())?;
        group.append_child(&draw_label(&document, &layout, (0, map_radius), "+r")?.into())?;
        group.append_child(&draw_label(&document, &layout, (0, -map_radius), "-r")?.into())?;
        canvas.append_child(&group)?;

        let tiles = document.create_element_ns(SVG_NS, "g")?;
        tiles.set_id("tiles");
        for tile in map.tiles() {
            tiles.append_child(&draw_tile(&document, &layout, tile.id, tile.pos, tile.dir)?.into())?;
        }
        canvas.append_child(&tiles)?;

        let selected = SelectedHex::new(&document, &layout)?;
        canvas.append_child(selected.as_ref())?;

        let selected_menu = SelectedMenu::new(&document, &layout, (2, 2).into())?;
        selected_menu.set_hidden(true);
        canvas.append_child(selected_menu.as_ref())?;

        let active = ActiveHex::new(&document, &layout)?;
        canvas.append_child(active.as_ref())?;

        let dragged = None;

        // add drag-n-drop event handlers to canvas element
        let callback = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
            let pos = check!(mouse_position(&event));
            nuts::publish(UpdateSelectedEvent { pos });
            nuts::publish(DragMapBeginEvent { pos });
        }) as Box<dyn Fn(_)>);
        canvas.add_event_listener_with_callback("mousedown", callback.as_ref().unchecked_ref()).unwrap();
        callback.forget();

        let dragged_mousemove_cb = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
            event.prevent_default();
            let pos = check!(mouse_position(&event));
            nuts::publish(DragMapMoveEvent { pos });
        }) as Box<dyn Fn(_)>);

        let dragged_mouseup_cb = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
            let pos = check!(mouse_position(&event));
            nuts::publish(DragMapEndEvent { pos });
            nuts::publish(UpdateSelectedEvent { pos });
        }) as Box<dyn Fn(_)>);

        let dragged_mouseleave_cb = Closure::wrap(Box::new(move |_event: web_sys::MouseEvent| {
            nuts::publish(DragMapCancelEvent);
        }) as Box<dyn Fn(_)>);

        let control = document.get_element_by_id("clear-map-button").unwrap()
            .dyn_into::<web_sys::HtmlElement>().unwrap();
        let callback = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::publish(ClearMapEvent);
        }) as Box<dyn Fn(_)>);
        control.add_event_listener_with_callback("click", callback.as_ref().unchecked_ref()).unwrap();
        callback.forget();

        // add event handler(s) to file input element
        let input = document.get_element_by_id("upload").unwrap()
            .dyn_into::<web_sys::HtmlElement>().unwrap();
        let callback = Closure::wrap(Box::new(move |event: web_sys::Event| {
            let input = check!(event.target()
                .and_then(|target| target.dyn_into::<web_sys::HtmlInputElement>().ok()));
            let file = check!(input.files().and_then(|list| list.item(0)));
            let reader = check!(web_sys::FileReader::new().ok());
            let cb_reader = reader.clone();
            let callback = Closure::wrap(Box::new(move |_event: web_sys::Event| {
                let result = check!(cb_reader.result().ok());
                let data = check!(result.as_string());
                info!("input file data: {}", &data);
                nuts::publish(ImportFileEvent { data });
            }) as Box<dyn FnMut(_)>);
            reader.add_event_listener_with_callback("load", callback.as_ref().unchecked_ref()).unwrap();
            reader.read_as_text(&file).expect("file not readable");
            callback.forget();
        }) as Box<dyn Fn(_)>);
        input.add_event_listener_with_callback("change", callback.as_ref().unchecked_ref()).unwrap();
        callback.forget();

        let control = document.get_element_by_id("download-map-button").unwrap()
            .dyn_into::<web_sys::HtmlElement>().unwrap();
        let callback = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::publish(ExportFileEvent);
        }) as Box<dyn Fn(_)>);
        control.add_event_listener_with_callback("click", callback.as_ref().unchecked_ref()).unwrap();
        callback.forget();

        Ok(MapView {
            layout, map, canvas, tiles, selected, selected_menu, active,
            dragged, dragged_mousemove_cb, dragged_mouseup_cb,
            dragged_mouseleave_cb
        })
    }

    pub fn import_file(&mut self, data: &str) {
        let mut map = match import::import_rgt(data) {
            Ok(val) => val,
            Err(err) => {
                warn!("Cannot import file data: {}", err);
                return;
            },
        };
        map.align_center();
        self.clear_selected();
        self.map = map;
        self.update_map();
    }

    pub fn export_file(&mut self) {
        let data = match import::export_rgt(&self.map, "My Track") {
            Ok(val) => val,
            Err(err) => {
                warn!("Cannot emport file data: {}", err);
                return;
            },
        };
        let mut url = String::from("data:text/json;charset=utf-8,");
        url.push_str(&String::from(js_sys::encode_uri_component(&data)));

        let document = self.canvas.owner_document().unwrap();
        let anchor = check!(document.create_element("a").ok()
            .and_then(|elm| elm.dyn_into::<web_sys::HtmlElement>().ok()));
        anchor.set_hidden(true);
        check!(anchor.set_attribute("href", &url).ok());
        check!(anchor.set_attribute("download", "MyTrack.rgt").ok());
        check!(document.body().unwrap().append_child(&anchor).ok());
        anchor.click();
        check!(document.body().unwrap().remove_child(&anchor).ok());
    }

    pub fn insert_tile(&mut self, id: TileId, pos: Coordinate, dir: Direction) {
        self.map.insert(id, pos, dir);
        self.update_map();
    }

    pub fn append_tile(&mut self, id: TileId, hint: Option<ConnectionHint>) {
        self.map.append(id, hint);
        self.update_map();
    }

    pub fn align_center(&mut self) {
        self.clear_selected();
        self.map.align_center();
        self.update_map();
    }

    pub fn clear_map(&mut self) {
        self.clear_selected();
        self.map = Map::new();
        self.update_map();
    }

    pub fn update_map(&mut self) {
        let document = self.tiles.owner_document().unwrap();
        // remove all existing tiles
        let range = check!(document.create_range().ok());
        check!(range.select_node_contents(&self.tiles).ok());
        check!(range.delete_contents().ok());
        // then add updated tiles
        for tile in self.map.tiles() {
            if let Ok(el) = draw_tile(&document, &self.layout, tile.id, tile.pos, tile.dir) {
                self.tiles.append_child(&el).unwrap();
            }
        }
        self.active.update(&self.layout, self.map.active_pos(), self.map.active_dir());
    }

    pub fn clear_selected(&mut self) {
        self.selected.set_pos(&self.layout, None);
        self.selected.set_draggable(false);
        self.selected_menu.set_hidden(true);
    }

    pub fn update_selected(&mut self, pos: Point) {
        let pos = Coordinate::from_pixel_rounded(&self.layout, pos);
        info!("update selected: {:?}", pos);
        let tile = self.map.get(pos);
        if self.selected.pos() != Some(pos) || tile.is_some() {
            self.selected.set_pos(&self.layout, Some(pos));
        } else {
            self.selected.set_pos(&self.layout, None);
        }
        self.selected.set_draggable(tile.is_some());
        self.selected_menu.set_pos(&self.layout, pos);
        self.selected_menu.set_hidden(!tile.is_some());
    }

    pub fn rotate_selected_left(&mut self) {
        let tile = self.selected.pos()
            .and_then(|pos| self.map.get(pos).cloned());
        if let Some(tile) = tile {
            self.map.insert(tile.id, tile.pos, tile.dir.rotated_left());
            self.update_map();
        }
    }

    pub fn rotate_selected_right(&mut self) {
        let tile = self.selected.pos()
            .and_then(|pos| self.map.get(pos).cloned());
        if let Some(tile) = tile {
            self.map.insert(tile.id, tile.pos, tile.dir.rotated_right());
            self.update_map();
        }
    }

    pub fn remove_selected(&mut self) {
        if let Some(pos) = self.selected.pos() {
            self.map.remove(pos);
            self.update_map();
        }
        self.selected.set_draggable(false);
        self.selected_menu.set_hidden(true);
    }

    pub fn drag_begin(&mut self, pos: Point) {
        let pos = Coordinate::from_pixel_rounded(&self.layout, pos);
        if self.selected.pos() != Some(pos) {
            return;
        }
        if let Some(tile) = self.map.get(pos) {
            info!("drag begin: {:?}", tile);
            let document = check!(self.canvas.owner_document());
            let dragged = check!(DraggedTile::new(&document, &self.layout, tile.clone()).ok());
            // dragged element will be made visible later on mouse move to avoid flicker
            dragged.set_hidden(true);
            check!(self.canvas.append_child(dragged.as_ref()).ok());
            self.dragged = Some(dragged);

            check!(self.canvas.class_list().add_1("is-dragged").ok());
            self.selected.set_draggable(false);
            check!(self.canvas.add_event_listener_with_callback("mousemove",
                self.dragged_mousemove_cb.as_ref().unchecked_ref()).ok());
            check!(self.canvas.add_event_listener_with_callback("mouseup",
                self.dragged_mouseup_cb.as_ref().unchecked_ref()).ok());
            check!(self.canvas.add_event_listener_with_callback("mouseleave",
                self.dragged_mouseleave_cb.as_ref().unchecked_ref()).ok());
        }
    }

    pub fn drag_move(&mut self, pos: Point) {
        if let Some(ref dragged) = self.dragged {
            debug!("drag move: {:?}", pos);
            dragged.set_pos(pos);
            // make dragged element visible on first mouse move
            dragged.set_hidden(false);
        }
        // hide menu during drag operation
        self.selected_menu.set_hidden(true);
    }

    pub fn drag_end(&mut self, pos: Point, added_tile: Option<TileId>) {
        if let Some(ref dragged) = self.dragged {
            let pos = Coordinate::from_pixel_rounded(&self.layout, pos);
            let tile = dragged.tile();
            info!("drag end: {:?} -> {:?}", tile, pos);
            check!(self.canvas.remove_child(dragged.as_ref()).ok());
            if pos != tile.pos {
                self.map.remove(tile.pos);
                self.map.insert(tile.id, pos, tile.dir);
                self.update_map();
            }
        }
        self.dragged = None;

        if let Some(tile) = added_tile {
            let pos = Coordinate::from_pixel_rounded(&self.layout, pos);
            info!("drag end: {:?} -> {:?}", tile, pos);
            self.map.insert(tile, pos, Direction::A);
            self.update_map();
        }

        check!(self.canvas.class_list().remove_1("is-dragged").ok());
        self.selected.set_draggable(true);
        check!(self.canvas.remove_event_listener_with_callback("mousemove",
            self.dragged_mousemove_cb.as_ref().unchecked_ref()).ok());
        check!(self.canvas.remove_event_listener_with_callback("mouseup",
            self.dragged_mouseup_cb.as_ref().unchecked_ref()).ok());
        check!(self.canvas.remove_event_listener_with_callback("mouseleave",
            self.dragged_mouseleave_cb.as_ref().unchecked_ref()).ok());
    }

    pub fn drag_cancel(&mut self) {
        if let Some(ref dragged) = self.dragged {
            let tile = dragged.tile();
            info!("drag cancel: {:?}", tile);
            check!(self.canvas.remove_child(dragged.as_ref()).ok());
        }
        self.dragged = None;

        check!(self.canvas.class_list().remove_1("is-dragged").ok());
        self.selected.set_draggable(true);
        check!(self.canvas.remove_event_listener_with_callback("mousemove",
            self.dragged_mousemove_cb.as_ref().unchecked_ref()).ok());
        check!(self.canvas.remove_event_listener_with_callback("mouseup",
            self.dragged_mouseup_cb.as_ref().unchecked_ref()).ok());
        check!(self.canvas.remove_event_listener_with_callback("mouseleave",
            self.dragged_mouseleave_cb.as_ref().unchecked_ref()).ok());
    }
}

impl Drop for MapView {
    fn drop(&mut self) {
        self.canvas.remove_event_listener_with_callback("mousemove",
            self.dragged_mousemove_cb.as_ref().unchecked_ref()).unwrap();
        self.canvas.remove_event_listener_with_callback("mouseup",
            self.dragged_mouseup_cb.as_ref().unchecked_ref()).unwrap();
        self.canvas.remove_event_listener_with_callback("mouseleave",
            self.dragged_mouseleave_cb.as_ref().unchecked_ref()).unwrap();
    }
}

//----------------------------------------------------------------------------