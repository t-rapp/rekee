//----------------------------------------------------------------------------
//! Rendering of map objects and event handlers.
//
// $Id$
//----------------------------------------------------------------------------

use log::{warn, info};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Document, Element};

use crate::controller::*;
use crate::hexagon::*;
use crate::import;
use crate::tile::*;

//----------------------------------------------------------------------------

type Result<T> = std::result::Result<T, JsValue>;

fn define_grid_hex(document: &Document, layout: &Layout) -> Result<Element>
{
    let corners = layout.hexagon_corners((0, 0).into());
    let points: Vec<String> = corners.iter()
        .map(|p| *p - layout.origin())
        .map(|p| format!("{},{}", p.x(), p.y()))
        .collect();
    let hex = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "polygon")?;
    hex.set_id("hex");
    hex.set_attribute("points", &points.join(" "))?;
    Ok(hex)
}

fn use_grid_hex<C>(document: &Document, layout: &Layout, pos: C) -> Result<Element>
    where C: Into<Coordinate>
{
    let pos = pos.into().to_pixel(&layout);
    let hex = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "use")?;
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
    let poly = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "polygon")?;
    poly.set_attribute("points", &points.join(" "))?;

    let pos = pos.into().to_pixel(&layout);
    let hex = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "g")?;
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
    let menu = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "foreignObject")?;
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
    let label = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "text")?;
    label.set_attribute("class", "label")?;
    label.set_attribute("x", &pos.x().to_string())?;
    label.set_attribute("y", &pos.y().to_string())?;
    label.set_inner_html(text);
    Ok(label)
}

fn draw_tile<C, D>(document: &Document, layout: &Layout, id: TileId, pos: C, dir: D) -> Result<Element>
    where C: Into<Coordinate>, D: Into<Direction>
{
    let size = layout.size();
    let angle = dir.into().to_angle(&layout);
    let img = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "image")?;
    img.set_attribute("href", &format!("img/thumb-{}.png", id))?;
    img.set_attribute("width", &format!("{}", 2.0 * size.x()))?;
    img.set_attribute("height", &format!("{}", 2.0 * size.y()))?;
    img.set_attribute("transform", &format!("rotate({:.0}) translate({:.3} {:.3})", angle, -size.x(), -size.y()))?;

    let label = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "text")?;
    label.set_attribute("class", "label")?;
    label.set_attribute("x", "0")?;
    label.set_attribute("y", "0")?;
    label.set_inner_html(&id.base().to_string());

    let pos = pos.into().to_pixel(&layout);
    let tile = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "g")?;
    tile.set_attribute("id", &id.to_string())?;
    tile.set_attribute("class", "tile")?;
    tile.set_attribute("transform", &format!("translate({:.3} {:.3})", pos.x(), pos.y()))?;
    tile.append_child(&img)?;
    tile.append_child(&label)?;
    Ok(tile)
}

fn draw_dragged_tile<P, D>(document: &Document, layout: &Layout, id: TileId, pos: P, dir: D) -> Result<Element>
    where P: Into<Point>, D: Into<Direction>
{
    let size = layout.size();
    let angle = dir.into().to_angle(&layout);
    let img = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "image")?;
    img.set_attribute("href", &format!("img/thumb-{}.png", id))?;
    img.set_attribute("width", &format!("{}", 2.0 * size.x()))?;
    img.set_attribute("height", &format!("{}", 2.0 * size.y()))?;
    img.set_attribute("transform", &format!("rotate({:.0}) translate({:.3} {:.3})", angle, -size.x(), -size.y()))?;

    let pos = pos.into();
    let tile = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "g")?;
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

fn mouse_position(event: web_sys::MouseEvent) -> Option<Point> {
    let element = event.current_target()
        .and_then(|target| target.dyn_into::<web_sys::Element>().ok())?;
    let rect = element.get_bounding_client_rect();
    let x = event.client_x() as f32 - rect.left() as f32;
    let y = event.client_y() as f32 - rect.top() as f32;
    Some(Point(x, y))
}

macro_rules! check {
    ($e:expr) => {
        match $e {
            None => return,
            Some(val) => val,
        }
    };
}

//----------------------------------------------------------------------------

pub struct PageView {
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

impl PageView {
    pub fn new(parent: Element, layout: Layout) -> Result<Self> {
        let map = Map::new();
        let document = parent.owner_document().unwrap();

        // remove all pre-existing child nodes
        let range = document.create_range()?;
        range.select_node_contents(&parent)?;
        range.delete_contents()?;

        let canvas = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "svg")?;
        canvas.set_attribute("width", &format!("{}", 2.0 * layout.origin().x()))?;
        canvas.set_attribute("height", &format!("{}", 2.0 * layout.origin().y()))?;
        parent.append_child(&canvas)?;

        let defs = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "defs")?;
        defs.append_child(&define_grid_hex(&document, &layout)?.into())?;
        canvas.append_child(&defs)?;

        let group = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "g")?;
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

        let group = document.create_element_ns(Some("http://www.w3.org/2000/svg"), "g")?;
        group.set_id("tiles");
        for tile in map.tiles() {
            group.append_child(&draw_tile(&document, &layout, tile.id, tile.pos, tile.dir)?.into())?;
        }
        canvas.append_child(&group)?;

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
            let pos = check!(mouse_position(event));
            nuts::publish(UpdateSelectedEvent { pos });
            nuts::publish(DragBeginEvent { pos });
        }) as Box<dyn Fn(_)>);
        canvas.add_event_listener_with_callback("mousedown", callback.as_ref().unchecked_ref()).unwrap();
        callback.forget();

        let dragged_mousemove_cb = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
            event.prevent_default();
            let pos = check!(mouse_position(event));
            nuts::publish(DragMoveEvent { pos });
        }) as Box<dyn Fn(_)>);

        let dragged_mouseup_cb = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
            let pos = check!(mouse_position(event));
            nuts::publish(DragEndEvent { pos });
            nuts::publish(UpdateSelectedEvent { pos });
        }) as Box<dyn Fn(_)>);

        let dragged_mouseleave_cb = Closure::wrap(Box::new(move |_event: web_sys::MouseEvent| {
            nuts::publish(DragCancelEvent);
        }) as Box<dyn Fn(_)>);

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

        Ok(PageView {
            layout, map, canvas, tiles: group, selected, selected_pos,
            selected_menu, dragged_tile, dragged_mousemove_cb,
            dragged_mouseup_cb, dragged_mouseleave_cb, dragged: None
        })
    }

    pub fn import_file(&mut self, data: &str) {
        let map = match import::import_rgt(data) {
            Ok(val) => val,
            Err(err) => {
                warn!("Cannot import file data: {}", err);
                return;
            },
        };
        self.map = map;
        self.update_map();
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
        self.map.align_center();
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
            info!("drag move: {:?}", pos);
            check!(move_dragged_tile(dragged, pos).ok());
        } else if let Some(ref tile) = self.dragged_tile {
            // hide menu during drag operation
            check!(self.selected_menu.class_list().add_1("is-hidden").ok());
            // create missing dragged element on first mouse move
            let document = self.canvas.owner_document().unwrap();
            let dragged = check!(draw_dragged_tile(&document, &self.layout,
                tile.id, pos, tile.dir).ok());
            check!(self.canvas.append_child(&dragged).ok());
            self.dragged = Some(dragged);
        }
    }

    pub fn drag_end(&mut self, pos: Point) {
        if let Some(ref tile) = self.dragged_tile {
            let pos = Coordinate::from_pixel_rounded(&self.layout, pos);
            info!("drag end: {:?} -> {:?}", tile, pos);
            if pos != tile.pos {
                self.map.remove(tile.pos);
                self.map.insert(tile.id, pos, tile.dir);
                self.update_map();
            }
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

impl Drop for PageView {
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
