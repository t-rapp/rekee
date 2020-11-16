//----------------------------------------------------------------------------
//! Rendering of the map and according event handlers.
//
// $Id$
//----------------------------------------------------------------------------

use log::{warn, info, debug};
use js_sys::{self, JsString};
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

fn draw_hex<C>(document: &Document, layout: &Layout, pos: C) -> Result<Element>
    where C: Into<Coordinate>
{
    let corners = layout.hexagon_corners((0, 0).into());
    let points: Vec<String> = corners.iter()
        .map(|p| *p - layout.origin())
        .map(|p| format!("{},{}", p.x(), p.y()))
        .collect();
    let poly = document.create_element_ns(SVG_NS, "polygon")?;
    poly.set_attribute("points", &points.join(" "))?;

    let pos = pos.into().to_pixel(&layout);
    let hex = document.create_element_ns(SVG_NS, "g")?;
    hex.set_attribute("transform", &format!("translate({:.3} {:.3})", pos.x(), pos.y()))?;
    hex.append_child(&poly)?;
    Ok(hex)
}

fn move_hex<C>(hex: &Element, layout: &Layout, pos: C) -> Result<()>
    where C: Into<Coordinate>
{
    let pos = pos.into().to_pixel(&layout);
    hex.set_attribute("transform", &format!("translate({:.3} {:.3})", pos.x(), pos.y()))?;
    Ok(())
}

fn draw_selected_menu<C>(document: &Document, layout: &Layout, pos: C) -> Result<Element>
    where C: Into<Coordinate>
{
    let pos = pos.into().to_pixel(&layout);
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
    Ok(menu)
}

fn move_selected_menu<C>(menu: &Element, layout: &Layout, pos: C) -> Result<()>
    where C: Into<Coordinate>
{
    let pos = pos.into().to_pixel(&layout);
    menu.set_attribute("x", &(pos.x() - 60.0).to_string())?;
    menu.set_attribute("y", &(pos.y() + layout.size().y() - 8.0).to_string())?;
    Ok(())
}

fn draw_label<C>(document: &Document, layout: &Layout, pos: C, text: &str) -> Result<Element>
    where C: Into<Coordinate>
{
    let pos = pos.into().to_pixel(&layout);
    let label = document.create_element_ns(SVG_NS, "text")?;
    label.set_attribute("class", "label")?;
    label.set_attribute("x", &pos.x().to_string())?;
    label.set_attribute("y", &pos.y().to_string())?;
    label.set_inner_html(text);
    Ok(label)
}

fn draw_dragged_tile<P, D>(document: &Document, layout: &Layout, id: TileId, pos: P, dir: D) -> Result<Element>
    where P: Into<Point>, D: Into<Direction>
{
    let size = layout.size();
    let angle = dir.into().to_angle(&layout);
    let img = document.create_element_ns(SVG_NS, "image")?;
    img.set_attribute("href", &format!("img/thumb-{}.png", id))?;
    img.set_attribute("width", &format!("{}", 2.0 * size.x()))?;
    img.set_attribute("height", &format!("{}", 2.0 * size.y()))?;
    img.set_attribute("transform", &format!("rotate({:.0}) translate({:.3} {:.3})", angle, -size.x(), -size.y()))?;

    let pos = pos.into();
    let tile = document.create_element_ns(SVG_NS, "g")?;
    tile.set_attribute("id", "dragged")?;
    tile.set_attribute("class", "tile")?;
    tile.set_attribute("transform", &format!("translate({:.3} {:.3})", pos.x(), pos.y()))?;
    tile.append_child(&img)?;
    Ok(tile)
}

fn move_dragged_tile<P>(tile: &Element, pos: P) -> Result<()>
    where P: Into<Point>
{
    let pos = pos.into();
    tile.set_attribute("transform", &format!("translate({:.3} {:.3})", pos.x(), pos.y()))?;
    Ok(())
}

//----------------------------------------------------------------------------

pub struct MapView {
    layout: Layout,
    map: Map,
    canvas: Element,
    tiles: Element,
    selected_pos: Option<Coordinate>,
    selected: Element,
    selected_menu: Element,
    dragged_tile: Option<PlacedTile>,
    dragged_mousemove_cb: Closure<dyn Fn(web_sys::MouseEvent)>,
    dragged_mouseup_cb: Closure<dyn Fn(web_sys::MouseEvent)>,
    dragged_mouseleave_cb: Closure<dyn Fn(web_sys::MouseEvent)>,
    dragged: Option<Element>,
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

        let selected_pos = None;
        let selected = draw_hex(&document, &layout, (2, 2))?;
        selected.set_id("selected");
        selected.class_list().add_1("is-hidden")?;
        canvas.append_child(&selected)?;

        let selected_menu = draw_selected_menu(&document, &layout, (2, 2))?;
        selected_menu.class_list().add_1("is-hidden")?;
        canvas.append_child(&selected_menu)?;

        let dragged_tile = None;

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
            layout, map, canvas, tiles, selected, selected_pos, selected_menu,
            dragged_tile, dragged_mousemove_cb, dragged_mouseup_cb,
            dragged_mouseleave_cb, dragged: None
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
        let mut url = JsString::from("data:text/json;charset=utf-8,");
        url = url.concat(&js_sys::encode_uri_component(&data));
        let url: String = url.into();

        let document = self.canvas.owner_document().unwrap();
        let anchor = check!(document.create_element("a").ok()
            .and_then(|elm| elm.dyn_into::<web_sys::HtmlElement>().ok()));
        check!(anchor.set_attribute("class", "is-hidden").ok());
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
    }

    pub fn clear_selected(&mut self) {
        self.selected_pos = None;
        check!(self.selected.class_list().add_1("is-hidden").ok());
        check!(self.selected.class_list().remove_1("is-draggable").ok());
        check!(self.selected_menu.class_list().add_1("is-hidden").ok());
    }

    pub fn update_selected(&mut self, pos: Point) {
        let pos = Coordinate::from_pixel_rounded(&self.layout, pos);
        info!("update selected: {:?}", pos);
        let tile = self.map.get(pos);
        if self.selected_pos != Some(pos) {
            check!(move_hex(&self.selected, &self.layout, pos).ok());
            check!(move_selected_menu(&self.selected_menu, &self.layout, pos).ok());
        }
        if self.selected_pos != Some(pos) || tile.is_some() {
            self.selected_pos = Some(pos);
            check!(self.selected.class_list().remove_1("is-hidden").ok());
        } else {
            self.selected_pos = None;
            check!(self.selected.class_list().add_1("is-hidden").ok());
        }
        if tile.is_some() {
            check!(self.selected.class_list().add_1("is-draggable").ok());
            check!(self.selected_menu.class_list().remove_1("is-hidden").ok());
        } else {
            check!(self.selected.class_list().remove_1("is-draggable").ok());
            check!(self.selected_menu.class_list().add_1("is-hidden").ok());
        }
    }

    pub fn rotate_selected_left(&mut self) {
        let tile = self.selected_pos
            .and_then(|pos| self.map.get(pos).cloned());
        if let Some(tile) = tile {
            self.map.insert(tile.id, tile.pos, tile.dir.rotated_left());
            self.update_map();
        }
    }

    pub fn rotate_selected_right(&mut self) {
        let tile = self.selected_pos
            .and_then(|pos| self.map.get(pos).cloned());
        if let Some(tile) = tile {
            self.map.insert(tile.id, tile.pos, tile.dir.rotated_right());
            self.update_map();
        }
    }

    pub fn remove_selected(&mut self) {
        if let Some(pos) = self.selected_pos {
            self.map.remove(pos);
            self.update_map();
        }
        check!(self.selected.class_list().remove_1("is-draggable").ok());
        check!(self.selected_menu.class_list().add_1("is-hidden").ok());
    }

    pub fn drag_begin(&mut self, pos: Point) {
        let pos = Coordinate::from_pixel_rounded(&self.layout, pos);
        if self.selected_pos != Some(pos) {
            return;
        }
        if let Some(tile) = self.map.get(pos) {
            info!("drag begin: {:?}", tile);
            self.dragged_tile = Some(tile.clone());
            // dragged element will be created later on mouse move to avoid flicker

            check!(self.canvas.class_list().add_1("is-dragged").ok());
            check!(self.selected.class_list().remove_1("is-draggable").ok());
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
            check!(move_dragged_tile(dragged, pos).ok());
        } else if let Some(ref tile) = self.dragged_tile {
            // create missing dragged element on first mouse move
            let document = self.canvas.owner_document().unwrap();
            let dragged = check!(draw_dragged_tile(&document, &self.layout,
                tile.id, pos, tile.dir).ok());
            check!(self.canvas.append_child(&dragged).ok());
            self.dragged = Some(dragged);
        }
        // hide menu during drag operation
        check!(self.selected_menu.class_list().add_1("is-hidden").ok());
    }

    pub fn drag_end(&mut self, pos: Point, added_tile: Option<TileId>) {
        if let Some(ref tile) = self.dragged_tile {
            let pos = Coordinate::from_pixel_rounded(&self.layout, pos);
            info!("drag end: {:?} -> {:?}", tile, pos);
            if pos != tile.pos {
                self.map.remove(tile.pos);
                self.map.insert(tile.id, pos, tile.dir);
                self.update_map();
            }
        }
        if let Some(tile) = added_tile {
            let pos = Coordinate::from_pixel_rounded(&self.layout, pos);
            info!("drag end: {:?} -> {:?}", tile, pos);
            self.map.insert(tile, pos, Direction::A);
            self.update_map();
        }
        self.dragged_tile = None;
        if let Some(ref dragged) = self.dragged {
            check!(self.canvas.remove_child(dragged).ok());
        }
        self.dragged = None;

        check!(self.canvas.class_list().remove_1("is-dragged").ok());
        check!(self.selected.class_list().add_1("is-draggable").ok());
        check!(self.canvas.remove_event_listener_with_callback("mousemove",
            self.dragged_mousemove_cb.as_ref().unchecked_ref()).ok());
        check!(self.canvas.remove_event_listener_with_callback("mouseup",
            self.dragged_mouseup_cb.as_ref().unchecked_ref()).ok());
        check!(self.canvas.remove_event_listener_with_callback("mouseleave",
            self.dragged_mouseleave_cb.as_ref().unchecked_ref()).ok());
    }

    pub fn drag_cancel(&mut self) {
        if let Some(ref tile) = self.dragged_tile {
            info!("drag cancel: {:?}", tile);
        }
        self.dragged_tile = None;
        if let Some(ref dragged) = self.dragged {
            check!(self.canvas.remove_child(dragged).ok());
        }
        self.dragged = None;

        check!(self.canvas.class_list().remove_1("is-dragged").ok());
        check!(self.selected.class_list().add_1("is-draggable").ok());
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
