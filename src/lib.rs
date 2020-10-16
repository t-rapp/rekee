//----------------------------------------------------------------------------
//! Library providing track editor functions for the Rekee web application.
//
// $Id$
//----------------------------------------------------------------------------

#![allow(dead_code)]

use log::{warn, info};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Document, Element};

pub mod hexagon;
use hexagon::*;

mod import;
mod logger;

mod tile;
use tile::*;

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

macro_rules! check {
    ($e:expr) => {
        match $e {
            None => return,
            Some(val) => val,
        }
    };
}

//----------------------------------------------------------------------------

struct ImportFileEvent {
    data: String,
}

struct InsertTileEvent {
    id: TileId,
    pos: Coordinate,
    dir: Direction,
}

struct AppendTileEvent {
    id: TileId,
    hint: Option<ConnectionHint>,
}

struct AlignCenterEvent;

struct UpdateMapEvent;

struct UpdateSelectedEvent {
    pos: Point,
}

struct PageState {
    layout: Layout,
    map: Map,
    tiles: Element,
    selected: Element,
}

impl PageState {
    fn new(parent: Element, layout: Layout) -> Result<Self> {
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

        let selected = draw_hex(&document, &layout, (2, 2))?;
        selected.set_id("selected");
        selected.class_list().add_1("is-hidden")?;
        canvas.append_child(&selected)?;

        // add event handler to canvas element
        let callback = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
            let element = check!(event.current_target()
                .and_then(|trg| trg.dyn_into::<web_sys::Element>().ok()));
            let rect = element.get_bounding_client_rect();
            let x = event.client_x() as f32 - rect.left() as f32;
            let y = event.client_y() as f32 - rect.top() as f32;
            nuts::publish(UpdateSelectedEvent { pos: Point(x, y) });
        }) as Box<dyn Fn(_)>);
        canvas.add_event_listener_with_callback("click", callback.as_ref().unchecked_ref()).unwrap();
        callback.forget();

        Ok(PageState { layout, map, tiles: group, selected })
    }

    fn import_file(&mut self, event: &ImportFileEvent) {
        let map = match import::import_rgt(&event.data) {
            Ok(val) => val,
            Err(err) => {
                warn!("Cannot import file data: {}", err);
                return;
            },
        };
        self.map = map;
        nuts::publish(UpdateMapEvent);
    }

    fn insert_tile(&mut self, event: &InsertTileEvent) {
        self.map.insert(event.id, event.pos, event.dir);
        nuts::publish(UpdateMapEvent);
    }

    fn append_tile(&mut self, event: &AppendTileEvent) {
        self.map.append(event.id, event.hint);
        nuts::publish(UpdateMapEvent);
    }

    fn align_center(&mut self, _event: &AlignCenterEvent) {
        self.map.align_center();
        nuts::publish(UpdateMapEvent);
    }

    fn update_map(&mut self, _event: &UpdateMapEvent) {
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

    fn update_selected(&mut self, event: &UpdateSelectedEvent) {
        let pos = Coordinate::from_pixel_rounded(&self.layout, event.pos);
        check!(move_hex(&self.selected, &self.layout, pos).ok());
        check!(self.selected.class_list().remove_1("is-hidden").ok());
    }
}

//----------------------------------------------------------------------------

#[wasm_bindgen]
pub fn main() -> Result<()> {
    logger::init().unwrap();

    let document = web_sys::window().unwrap().document().unwrap();
    let parent = document.get_element_by_id("main").
        ok_or_else(|| "Cannot find '#main' parent element for page")?;

    let layout = Layout::new(Orientation::pointy(), Point(40.0, 40.0), Point(320.0, 300.0));
    let page = nuts::new_activity(PageState::new(parent, layout)?);
    page.subscribe(PageState::import_file);
    page.subscribe(PageState::insert_tile);
    page.subscribe(PageState::append_tile);
    page.subscribe(PageState::align_center);
    page.subscribe(PageState::update_map);
    page.subscribe(PageState::update_selected);

    nuts::publish(InsertTileEvent { id: tile!(102, b), pos: (0, 0).into(), dir: Direction::D });
    nuts::publish(AppendTileEvent { id: tile!(104, b), hint: None });
    nuts::publish(AppendTileEvent { id: tile!(113, b), hint: "R".parse().ok() });
    nuts::publish(AppendTileEvent { id: tile!(117, b), hint: "r".parse().ok() });
    nuts::publish(AppendTileEvent { id: tile!(114, b), hint: "R".parse().ok() });
    nuts::publish(AppendTileEvent { id: tile!(115, b), hint: "L".parse().ok() });
    nuts::publish(AppendTileEvent { id: tile!(115, b), hint: "l".parse().ok() });
    nuts::publish(AppendTileEvent { id: tile!(108, b), hint: "r".parse().ok() });
    nuts::publish(AppendTileEvent { id: tile!(110, b), hint: "L".parse().ok() });
    nuts::publish(AppendTileEvent { id: tile!(107, b), hint: "R".parse().ok() });
    nuts::publish(InsertTileEvent { id: tile!(101), pos: (0, -2).into(), dir: Direction::D});
    nuts::publish(AlignCenterEvent);

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

    Ok(())
}

//----------------------------------------------------------------------------
