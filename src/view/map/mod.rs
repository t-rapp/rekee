//----------------------------------------------------------------------------
//! Rendering of the map and according event handlers.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use serde::{Serialize, Deserialize};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Document, Element};

use crate::check;
use crate::controller::{
    AlignCenterEvent, ClearMapEvent, ExportFileEvent, ExportImageEvent,
    ImportFileEvent, RemoveSelectedTileEvent, RotateMapLeftEvent,
    RotateMapRightEvent, RotateSelectedTileLeftEvent,
    RotateSelectedTileRightEvent, SaveSettingsEvent, ToggleTileLabelsEvent,
    UpdateAuthorEvent, UpdateSelectedTileEvent, UpdateTileUsageEvent,
    UpdateTitleEvent
};
use crate::controller::map::*;
use crate::controller::map_config::ShowMapConfigEvent;
use crate::controller::map_detail::ShowMapDetailEvent;
use crate::controller::track_info::ShowTrackInfoEvent;
use crate::hexagon::{Direction, Rect};
use crate::import;
use crate::map::{Map, PlacedTile, PlacedToken};
use crate::tile::{ConnectionHint, TileId};
use super::*;

pub mod config;
pub mod detail;
mod polar;

//----------------------------------------------------------------------------

const MAP_STYLE: &str = include_str!("style.css");
const MAP_PADDING: f32 = 15.0;

fn draw_grid_hex(document: &Document, layout: &Layout, pos: Coordinate) -> Result<Element> {
    let corners = layout.hexagon_corners(pos);
    let points: Vec<String> = corners.iter()
        .map(|p| format!("{:.1},{:.1}", p.x(), p.y()))
        .collect();
    let hex = document.create_element_ns(SVG_NS, "polygon")?;
    hex.set_attribute("class", "hex")?;
    hex.set_attribute("points", &points.join(" "))?;
    Ok(hex)
}

fn draw_label(document: &Document, layout: &Layout, pos: Coordinate, text: &str) -> Result<Element> {
    let pos = pos.to_pixel(layout);
    let label = document.create_element_ns(SVG_NS, "text")?;
    label.set_attribute("class", "label")?;
    label.set_attribute("x", &pos.x().to_string())?;
    label.set_attribute("y", &pos.y().to_string())?;
    label.append_child(&document.create_text_node(text))?;
    Ok(label)
}

//----------------------------------------------------------------------------

struct TitleInput {
    inner: web_sys::HtmlInputElement,
    change_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl TitleInput {
    fn new(document: &Document) -> Result<Self> {
        let inner = document.query_selector("#map-title .input")?
            .and_then(|elm| elm.dyn_into::<web_sys::HtmlInputElement>().ok())
            .ok_or("Cannot find map title input element")?;

        let change_cb = Closure::wrap(Box::new(move |event: web_sys::Event| {
            let input = check!(event.target()
                .and_then(|target| target.dyn_into::<web_sys::HtmlInputElement>().ok()));
            let title = input.value();
            debug!("map title changed: {}", &title);
            nuts::publish(UpdateTitleEvent { title });
        }) as Box<dyn Fn(_)>);
        inner.add_event_listener_with_callback("change", change_cb.as_ref().unchecked_ref()).unwrap();

        Ok(TitleInput { inner, change_cb })
    }

    fn set_value(&self, value: &str) {
        self.inner.set_value(value)
    }
}

impl AsRef<Element> for TitleInput {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

impl Drop for TitleInput {
    fn drop(&mut self) {
        let _ = self.inner.remove_event_listener_with_callback("change",
            self.change_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------

struct AuthorInput {
    inner: web_sys::HtmlInputElement,
    change_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl AuthorInput {
    fn new(document: &Document) -> Result<Self> {
        let inner = document.query_selector("#map-author .input")?
            .and_then(|elm| elm.dyn_into::<web_sys::HtmlInputElement>().ok())
            .ok_or("Cannot find map author input element")?;

        let change_cb = Closure::wrap(Box::new(move |event: web_sys::Event| {
            let input = check!(event.target()
                .and_then(|target| target.dyn_into::<web_sys::HtmlInputElement>().ok()));
            let author = input.value();
            debug!("map author changed: {}", &author);
            nuts::publish(UpdateAuthorEvent { author });
        }) as Box<dyn Fn(_)>);
        inner.add_event_listener_with_callback("change", change_cb.as_ref().unchecked_ref())?;

        Ok(AuthorInput { inner, change_cb })
    }

    fn set_value(&self, value: &str) {
        self.inner.set_value(value)
    }
}

impl AsRef<Element> for AuthorInput {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

impl Drop for AuthorInput {
    fn drop(&mut self) {
        let _ = self.inner.remove_event_listener_with_callback("change",
            self.change_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------

struct SelectedHex {
    inner: Element,
    pos: Option<Coordinate>,
    dblclick_cb: Closure<dyn Fn(web_sys::MouseEvent)>,
}

impl SelectedHex {
    fn new(document: &Document, layout: &Layout) -> Result<Self> {
        let corners = layout.hexagon_corners((0, 0).into());
        let points: Vec<String> = corners.iter()
            .map(|p| *p - layout.origin())
            .map(|p| format!("{:.1},{:.1}", p.x(), p.y()))
            .collect();
        let poly = document.create_element_ns(SVG_NS, "polygon")?;
        poly.set_attribute("points", &points.join(" "))?;

        let hex = document.create_element_ns(SVG_NS, "g")?;
        hex.set_attribute("class", "selected-hex is-print-hidden")?;
        hex.set_hidden(true);
        hex.append_child(&poly)?;

        let dblclick_cb = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
            event.prevent_default();
            event.stop_propagation();
            nuts::send_to::<MapController, _>(SaveSettingsEvent);
            nuts::publish(ShowMapDetailEvent);
        }) as Box<dyn Fn(_)>);
        hex.add_event_listener_with_callback("dblclick", dblclick_cb.as_ref().unchecked_ref())?;

        Ok(SelectedHex { inner: hex, pos: None, dblclick_cb })
    }

    fn pos(&self) -> Option<Coordinate> {
        self.pos
    }

    #[allow(clippy::unnecessary_unwrap)] // false positive, see issue rust-lang/rust-clippy#4530
    fn set_pos(&mut self, layout: &Layout, pos: Option<Coordinate>) {
        if pos.is_some() && pos != self.pos {
            let pos = pos.unwrap().to_pixel(layout);
            let transform = format!("translate({:.1} {:.1})", pos.x(), pos.y());
            check!(self.inner.set_attribute("transform", &transform).ok());
        }
        self.set_hidden(pos.is_none());
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

impl Drop for SelectedHex {
    fn drop(&mut self) {
        let _ = self.inner.remove_event_listener_with_callback("dblclick",
            self.dblclick_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------

struct SelectedMenu {
    inner: Element,
}

impl SelectedMenu {
    fn new(document: &Document, layout: &Layout, pos: Coordinate) -> Result<Self> {
        let pos = pos.to_pixel(layout);
        let menu = document.create_element_ns(SVG_NS, "foreignObject")?;
        menu.set_attribute("class", "is-print-hidden")?;
        menu.set_attribute("x", &(pos.x() - 60.0).to_string())?;
        menu.set_attribute("y", &(pos.y() + layout.size().y() - 8.0).to_string())?;
        menu.set_attribute("width", "120")?;
        menu.set_attribute("height", "40")?;

        if let Some(btn) = document.get_element_by_id("rotate-selected-left-button") {
            let callback = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
                event.prevent_default();
                event.stop_propagation();
                nuts::publish(RotateSelectedTileLeftEvent);
            }) as Box<dyn Fn(_)>);
            btn.add_event_listener_with_callback("mousedown", callback.as_ref().unchecked_ref()).unwrap();
            callback.forget();
        }

        if let Some(btn) = document.get_element_by_id("rotate-selected-right-button") {
            let callback = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
                event.prevent_default();
                event.stop_propagation();
                nuts::publish(RotateSelectedTileRightEvent);
            }) as Box<dyn Fn(_)>);
            btn.add_event_listener_with_callback("mousedown", callback.as_ref().unchecked_ref()).unwrap();
            callback.forget();
        }

        if let Some(btn) = document.get_element_by_id("show-map-detail-button") {
            let callback = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
                event.prevent_default();
                event.stop_propagation();
                nuts::send_to::<MapController, _>(SaveSettingsEvent);
                nuts::publish(ShowMapDetailEvent);
            }) as Box<dyn Fn(_)>);
            btn.add_event_listener_with_callback("mousedown", callback.as_ref().unchecked_ref()).unwrap();
            callback.forget();
        }

        if let Some(btn) = document.get_element_by_id("remove-selected-button") {
            let callback = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
                event.prevent_default();
                event.stop_propagation();
                nuts::publish(RemoveSelectedTileEvent);
            }) as Box<dyn Fn(_)>);
            btn.add_event_listener_with_callback("mousedown", callback.as_ref().unchecked_ref()).unwrap();
            callback.forget();
        }

        // extract menu element from document after all buttons have been assigned,
        // otherwise get_element_by_id() would not have been able to find the buttons
        let inner = document.get_element_by_id("selected-menu")
            .ok_or("Cannot find '#selected-menu' element within page")?;
        menu.append_child(&inner)?;

        Ok(SelectedMenu { inner: menu })
    }

    fn set_pos(&self, layout: &Layout, pos: Coordinate) {
        let pos = pos.to_pixel(layout);
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
    img: Element,
    pos: Coordinate,
    dir: Direction,
}

impl ActiveHex {
    fn new(document: &Document, layout: &Layout) -> Result<Self> {
        let pos = Coordinate::new(0, 0);
        let dir = Direction::D;

        let img = document.create_element_ns(SVG_NS, "image")?;
        img.set_attribute("href", "arrow-down-circle.svg")?;
        let size = Point(16.0, 16.0);
        img.set_attribute("width", &format!("{}", 2.0 * size.x()))?;
        img.set_attribute("height", &format!("{}", 2.0 * size.y()))?;
        img.set_attribute("transform", &format!("translate({:.1} {:.1})", -size.x(), -size.y()))?;

        let inner = document.create_element_ns(SVG_NS, "g")?;
        inner.set_attribute("class", "active-hex is-print-hidden")?;
        let inner_pos = pos.to_pixel(layout);
        let angle = layout.direction_to_angle(dir);
        let transform = format!("translate({:.1} {:.1}) rotate({:.0})", inner_pos.x(), inner_pos.y(), angle);
        inner.set_attribute("transform", &transform)?;
        inner.append_child(&img)?;

        Ok(ActiveHex { inner, img, pos, dir })
    }

    fn update(&mut self, layout: &Layout, pos: Coordinate, dir: Direction) {
        if pos != self.pos || dir != self.dir {
            let pos = pos.to_pixel(layout);
            let angle = layout.direction_to_angle(dir);
            let transform = format!("translate({:.1} {:.1}) rotate({:.0})", pos.x(), pos.y(), angle);
            check!(self.inner.set_attribute("transform", &transform).ok());
        }
        self.pos = pos;
        self.dir = dir;
    }

    fn set_hint(&mut self, hint: Option<ConnectionHint>) {
        let href = match hint {
            Some(ConnectionHint::Left) =>
                "arrow-return-left-circle.svg",
            Some(ConnectionHint::Right) =>
                "arrow-return-right-circle.svg",
            _ =>
                "arrow-down-circle.svg",
        };
        check!(self.img.set_attribute("href", href).ok());
    }
}

impl AsRef<Element> for ActiveHex {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

//----------------------------------------------------------------------------

struct DraggedTile {
    inner: TileImageElement,
    tile: PlacedTile,
}

impl DraggedTile {
    fn new(document: &Document, layout: &Layout, tile: PlacedTile) -> Result<Self> {
        let inner = TileImageElement::new(document, layout, &tile)?;
        inner.set_id("dragged");

        Ok(DraggedTile{ inner, tile })
    }

    fn tile(&self) -> &PlacedTile {
        &self.tile
    }

    fn set_pos(&self, pos: Point) {
        check!(self.inner.set_attribute("transform",
            &format!("translate({:.1} {:.1})", pos.x(), pos.y())).ok());
    }
}

impl AsRef<Element> for DraggedTile {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

//----------------------------------------------------------------------------

#[derive(Default, Serialize, Deserialize)]
#[serde(default)]
pub struct MapSettings {
    #[serde(flatten)]
    pub map: Map,
    pub selected: Option<Coordinate>,
    pub background_grid_visible: bool,
    pub tile_labels_visible: bool,
}

//----------------------------------------------------------------------------

pub struct MapView {
    layout: Layout,
    map: Map,
    canvas: Element,
    canvas_viewbox: Rect,
    grid: Element,
    tiles: Element,
    tokens: Element,
    labels: Element,
    title: TitleInput,
    author: AuthorInput,
    selected: SelectedHex,
    selected_menu: SelectedMenu,
    active: ActiveHex,
    tile_labels_visible: bool,
    keychange_cb: Closure<dyn Fn(web_sys::KeyboardEvent)>,
    dragged: Option<DraggedTile>,
    dragged_mousemove_cb: Closure<dyn Fn(web_sys::MouseEvent)>,
    dragged_mouseup_cb: Closure<dyn Fn(web_sys::MouseEvent)>,
    dragged_mouseleave_cb: Closure<dyn Fn(web_sys::MouseEvent)>,
    document_title: String,
    download_button: web_sys::HtmlElement,
    export_button: web_sys::HtmlElement,
}

impl MapView {
    pub fn new(parent: Element, layout: &Layout) -> Result<Self> {
        let document = parent.owner_document().unwrap();
        let layout = layout.clone();
        let map = Map::new();

        let width = (2.0 * layout.origin().x()).round();
        let height = (2.0 * layout.origin().y()).round();
        let canvas_viewbox = Rect::new(0.0, 0.0, width, height);

        let canvas = document.create_element_ns(SVG_NS, "svg")?;
        canvas.set_id("map");
        canvas.set_attribute("width", &format!("{}px", canvas_viewbox.width as i32))?;
        canvas.set_attribute("height", &format!("{}px", canvas_viewbox.height as i32))?;
        canvas.set_attribute("viewBox", &format!("{} {} {} {}",
            canvas_viewbox.left as i32, canvas_viewbox.top as i32,
            canvas_viewbox.width as i32, canvas_viewbox.height as i32))?;
        canvas.set_attribute("xmlns", SVG_NS_STR)?;
        parent.append_child(&canvas)?;

        let style = document.create_element_ns(SVG_NS, "style")?;
        style.append_child(&document.create_text_node(MAP_STYLE))?;
        style.append_child(&document.create_text_node(TILE_STYLE))?;
        canvas.append_child(&style)?;

        let grid = document.create_element_ns(SVG_NS, "g")?;
        grid.set_attribute("class", "grid is-print-hidden")?;
        let map_radius = 8;
        for q in -map_radius..=map_radius {
            let r1 = i32::max(-map_radius, -q - map_radius);
            let r2 = i32::min(map_radius, -q + map_radius);
            for r in r1..=r2 {
                grid.append_child(&draw_grid_hex(&document, &layout, Coordinate::new(q, r))?.into())?;
            }
        }
        // add some hexagon grid axis labels when compiling in development mode
        if cfg!(debug_assertions) {
            let label_radius = 4;
            let label = draw_label(&document, &layout, Coordinate::new(label_radius, 0), "+q")?;
            grid.append_child(&label)?;
            let label = draw_label(&document, &layout, Coordinate::new(-label_radius, 0), "-q")?;
            grid.append_child(&label)?;
            let label = draw_label(&document, &layout, Coordinate::new(0, label_radius), "+r")?;
            grid.append_child(&label)?;
            let label = draw_label(&document, &layout, Coordinate::new(0, -label_radius), "-r")?;
            grid.append_child(&label)?;
        }
        canvas.append_child(&grid)?;

        let tiles = document.create_element_ns(SVG_NS, "g")?;
        tiles.set_attribute("class", "tiles")?;
        canvas.append_child(&tiles)?;

        let tokens = document.create_element_ns(SVG_NS, "g")?;
        tokens.set_attribute("class", "tokens")?;
        canvas.append_child(&tokens)?;

        let labels = document.create_element_ns(SVG_NS, "g")?;
        labels.set_attribute("class", "labels")?;
        canvas.append_child(&labels)?;

        let title = TitleInput::new(&document)?;
        title.set_value(map.title());

        let author = AuthorInput::new(&document)?;
        author.set_value(map.author());

        let selected = SelectedHex::new(&document, &layout)?;
        canvas.append_child(selected.as_ref())?;

        let selected_menu = SelectedMenu::new(&document, &layout, (2, 2).into())?;
        selected_menu.set_hidden(true);
        canvas.append_child(selected_menu.as_ref())?;

        let active = ActiveHex::new(&document, &layout)?;
        canvas.append_child(active.as_ref())?;

        let tile_labels_visible = true;
        let dragged = None;

        let keychange_cb = Closure::wrap(Box::new(move |event: web_sys::KeyboardEvent| {
            // ignore repeated keyboard events
            if event.repeat() {
                return;
            }
            // ignore keyboard events that have input controls as target
            let target = check!(event.target()
                .and_then(|target| target.dyn_into::<web_sys::HtmlElement>().ok()));
            let is_text_input_target =
                target.dyn_ref::<web_sys::HtmlInputElement>()
                    .map_or(false, |elm| !elm.read_only()) ||
                target.dyn_ref::<web_sys::HtmlTextAreaElement>()
                    .map_or(false, |elm| !elm.read_only()) ||
                target.has_type::<web_sys::HtmlSelectElement>() ||
                target.is_content_editable();
            if is_text_input_target {
                return;
            }
            if event.key().eq_ignore_ascii_case("l") {
                let inverted = match event.type_().as_ref() {
                    "keydown" => true,
                    "keyup" => false,
                    _ => return,
                };
                nuts::publish(ToggleTileLabelsEvent { inverted });
            }
        }) as Box<dyn Fn(_)>);
        document.add_event_listener_with_callback("keydown",
            keychange_cb.as_ref().unchecked_ref())?;
        document.add_event_listener_with_callback("keyup",
            keychange_cb.as_ref().unchecked_ref())?;

        // add drag-n-drop event handlers to canvas element
        let callback = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
            let pos = check!(mouse_position(&event));
            nuts::publish(UpdateSelectedTileEvent { pos });
            nuts::send_to::<MapController, _>(DragMapTileBeginEvent { pos });
        }) as Box<dyn Fn(_)>);
        canvas.add_event_listener_with_callback("mousedown", callback.as_ref().unchecked_ref()).unwrap();
        callback.forget();

        let dragged_mousemove_cb = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
            event.prevent_default();
            let pos = check!(mouse_position(&event));
            nuts::send_to::<MapController, _>(DragMapTileMoveEvent { pos });
        }) as Box<dyn Fn(_)>);

        let dragged_mouseup_cb = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
            let pos = check!(mouse_position(&event));
            nuts::send_to::<MapController, _>(DragMapTileEndEvent { pos });
            nuts::publish(UpdateSelectedTileEvent { pos });
        }) as Box<dyn Fn(_)>);

        let dragged_mouseleave_cb = Closure::wrap(Box::new(move |_event: web_sys::MouseEvent| {
            nuts::send_to::<MapController, _>(DragMapTileCancelEvent);
        }) as Box<dyn Fn(_)>);

        if let Some(btn) = document.get_element_by_id("align-center-button") {
            let callback = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
                event.prevent_default();
                event.stop_propagation();
                nuts::publish(AlignCenterEvent);
            }) as Box<dyn Fn(_)>);
            btn.add_event_listener_with_callback("click", callback.as_ref().unchecked_ref()).unwrap();
            callback.forget();
        }
        if let Some(btn) = document.get_element_by_id("rotate-map-left-button") {
            let callback = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
                event.prevent_default();
                event.stop_propagation();
                nuts::publish(RotateMapLeftEvent);
            }) as Box<dyn Fn(_)>);
            btn.add_event_listener_with_callback("click", callback.as_ref().unchecked_ref()).unwrap();
            callback.forget();
        }

        if let Some(btn) = document.get_element_by_id("rotate-map-right-button") {
            let callback = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
                event.prevent_default();
                event.stop_propagation();
                nuts::publish(RotateMapRightEvent);
            }) as Box<dyn Fn(_)>);
            btn.add_event_listener_with_callback("click", callback.as_ref().unchecked_ref()).unwrap();
            callback.forget();
        }

        if let Some(btn) = document.get_element_by_id("track-info-button") {
            let callback = Closure::wrap(Box::new(move |_event: web_sys::Event| {
                nuts::send_to::<MapController, _>(SaveSettingsEvent);
                nuts::publish(ShowTrackInfoEvent);
            }) as Box<dyn Fn(_)>);
            btn.add_event_listener_with_callback("click", callback.as_ref().unchecked_ref()).unwrap();
            callback.forget();
        }

        // remember original HTML document title (application name)
        let document_title = document.title();

        let control = document.get_element_by_id("clear-map-button").unwrap()
            .dyn_into::<web_sys::HtmlElement>().unwrap();
        let callback = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::publish(ClearMapEvent);
        }) as Box<dyn Fn(_)>);
        control.add_event_listener_with_callback("click", callback.as_ref().unchecked_ref()).unwrap();
        callback.forget();
        control.remove_attribute("disabled").unwrap();

        // add event handler(s) to file input element
        let input = document.get_element_by_id("upload").unwrap()
            .dyn_into::<web_sys::HtmlElement>().unwrap();
        let callback = Closure::wrap(Box::new(move |event: web_sys::Event| {
            let input = check!(event.target()
                .and_then(|target| target.dyn_into::<web_sys::HtmlInputElement>().ok()));
            let file = check!(input.files().and_then(|list| list.item(0)));
            let reader = check!(web_sys::FileReader::new().ok());
            let callback = Closure::wrap(Box::new({
                let reader = reader.clone();
                move |_event: web_sys::Event| {
                    let result = check!(reader.result().ok());
                    let data = check!(result.as_string());
                    debug!("input file data: {}", &data);
                    let map = match import::import_auto(&data) {
                        Ok(val) => val,
                        Err(err) => {
                            warn!("Cannot import file data: {}", err);
                            return;
                        },
                    };
                    nuts::publish(ImportFileEvent { map });
                }
            }) as Box<dyn FnMut(_)>);
            reader.add_event_listener_with_callback("load", callback.as_ref().unchecked_ref()).unwrap();
            reader.read_as_text(&file).expect("file not readable");
            callback.forget();
        }) as Box<dyn Fn(_)>);
        input.add_event_listener_with_callback("change", callback.as_ref().unchecked_ref()).unwrap();
        callback.forget();
        input.remove_attribute("disabled").unwrap();

        let download_button = document.get_element_by_id("download-map-button").unwrap()
            .dyn_into::<web_sys::HtmlElement>().unwrap();
        let callback = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::publish(ExportFileEvent);
        }) as Box<dyn Fn(_)>);
        download_button.add_event_listener_with_callback("click", callback.as_ref().unchecked_ref()).unwrap();
        callback.forget();

        let export_button = document.get_element_by_id("export-image-button").unwrap()
            .dyn_into::<web_sys::HtmlElement>().unwrap();
        let callback = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::send_to::<MapController, _>(SaveSettingsEvent);
            nuts::publish(ExportImageEvent);
        }) as Box<dyn Fn(_)>);
        export_button.add_event_listener_with_callback("click", callback.as_ref().unchecked_ref()).unwrap();
        callback.forget();

        let settings_button = document.get_element_by_id("map-config-button").unwrap()
            .dyn_into::<web_sys::HtmlElement>().unwrap();
        let callback = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::send_to::<MapController, _>(SaveSettingsEvent);
            nuts::publish(ShowMapConfigEvent);
        }) as Box<dyn Fn(_)>);
        settings_button.add_event_listener_with_callback("click", callback.as_ref().unchecked_ref()).unwrap();
        callback.forget();
        settings_button.remove_attribute("disabled").unwrap();

        let mut view = MapView {
            layout, map, canvas, canvas_viewbox, grid, tiles, tokens, labels,
            title, author, selected, selected_menu, active, tile_labels_visible,
            keychange_cb, dragged, dragged_mousemove_cb, dragged_mouseup_cb,
            dragged_mouseleave_cb, document_title, download_button,
            export_button
        };
        view.update_map();
        parent.set_hidden(false);

        Ok(view)
    }

    pub fn load_settings(&mut self, settings: &MapSettings) {
        self.map = settings.map.clone();
        self.grid.set_hidden(!settings.background_grid_visible);
        self.tile_labels_visible = settings.tile_labels_visible;
        self.update_map();
        self.inner_update_selected_tile(settings.selected.unwrap_or_default());
    }

    pub fn save_settings(&mut self) -> MapSettings {
        let map = self.map.clone();
        let selected = self.selected.pos();
        let background_grid_visible = !self.grid.hidden();
        let tile_labels_visible = self.tile_labels_visible;
        MapSettings { map, selected, background_grid_visible, tile_labels_visible }
    }

    pub fn import_file(&mut self, map: &Map) {
        let mut map = map.clone();
        map.align_center();
        self.clear_selected();
        self.map = map;
        self.update_map();
    }

    pub fn export_file(&mut self) {
        let data = match import::export_rgt(&self.map) {
            Ok(val) => val,
            Err(err) => {
                warn!("Cannot export file data: {}", err);
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
        let mut file_name = import::build_file_name(self.map.title());
        file_name.push_str(".rgt");
        check!(anchor.set_attribute("download", &file_name).ok());
        check!(document.body().unwrap().append_child(&anchor).ok());
        anchor.click();
        check!(document.body().unwrap().remove_child(&anchor).ok());
    }

    pub fn insert_tile(&mut self, id: TileId, pos: Coordinate, dir: Direction) {
        self.map.insert(id, pos, dir);
        self.update_map();
    }

    pub fn append_tile(&mut self, id: TileId, pos: Option<Coordinate>, hint: Option<ConnectionHint>) {
        self.map.append(id, pos, hint);
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

    pub fn rotate_map_left(&mut self) {
        self.map.rotate_left();
        if let Some(pos) = self.selected.pos() {
            self.inner_update_selected_tile(pos.rotated_left());
        }
        self.update_map();
    }

    pub fn rotate_map_right(&mut self) {
        self.map.rotate_right();
        if let Some(pos) = self.selected.pos() {
            self.inner_update_selected_tile(pos.rotated_right());
        }
        self.update_map();
    }

    fn update_map(&mut self) {
        let document = self.tiles.owner_document().unwrap();

        // calculate rectangular map area that is covered with tiles
        let mut map_area = Rect::new(f32::NAN, f32::NAN, 0.0, 0.0);
        for tile in self.map.tiles() {
            map_area = map_area.union(&self.layout.hexagon_rect(tile.pos));
        }
        map_area = map_area.with_padding(MAP_PADDING);
        map_area.left = map_area.left.floor();
        map_area.top = map_area.top.floor();
        map_area.width = map_area.width.ceil();
        map_area.height = map_area.height.ceil();
        debug!("map area: {:?}, layout origin: {:?}", map_area, self.layout.origin());

        // then update the SVG viewport properties
        let width = (2.0 * self.layout.origin().x()).round();
        let height = (2.0 * self.layout.origin().y()).round();
        let canvas_viewbox = Rect::new(0.0, 0.0, width, height)
            .union(&map_area);
        check!(self.canvas.set_attribute("width", &format!("{}px", canvas_viewbox.width as i32)).ok());
        check!(self.canvas.set_attribute("height", &format!("{}px", canvas_viewbox.height as i32)).ok());
        check!(self.canvas.set_attribute("viewBox", &format!("{} {} {} {}",
            canvas_viewbox.left as i32, canvas_viewbox.top as i32,
            canvas_viewbox.width as i32, canvas_viewbox.height as i32)).ok());
        self.canvas_viewbox = canvas_viewbox;

        // remove all existing tiles, tokens, and labels
        let range = check!(document.create_range().ok());
        check!(range.select_node_contents(&self.tiles).ok());
        check!(range.delete_contents().ok());
        check!(range.select_node_contents(&self.tokens).ok());
        check!(range.delete_contents().ok());
        check!(range.select_node_contents(&self.labels).ok());
        check!(range.delete_contents().ok());

        // then add updated tiles
        for tile in self.map.tiles() {
            if let Ok(el) = TileImageElement::new(&document, &self.layout, tile) {
                self.tiles.append_child(&el).unwrap();
            }
            for token in &tile.tokens {
                if let Ok(el) = TokenImageElement::new(&document, &self.layout, tile, token) {
                    self.tokens.append_child(&el).unwrap();
                }
            }
            if let Ok(el) = TileLabelElement::new(&document, &self.layout, tile) {
                self.labels.append_child(&el).unwrap();
            }
        }

        // update active tile append position
        if let Some(active_pos) = self.map.active_pos() {
            self.active.update(&self.layout, active_pos, self.map.active_dir());
            self.active.set_hidden(false);
        } else {
            self.active.set_hidden(true);
        }

        // update tile label visibility
        self.labels.set_hidden(!self.tile_labels_visible);

        // update title and author input controls
        self.title.set_value(self.map.title());
        self.author.set_value(self.map.author());

        // update HTML document title, use original title (application name) as a suffix
        let mut document_title = String::with_capacity(100);
        document_title.push_str(self.map.title());
        if !document_title.is_empty() {
            document_title.push_str(" - ");
        }
        document_title.push_str(&self.document_title);
        document.set_title(&document_title);

        // update button states
        if self.map.tiles().is_empty() {
            check!(self.download_button.set_attribute("disabled", "").ok());
            check!(self.export_button.set_attribute("disabled", "").ok());
        } else {
            check!(self.download_button.remove_attribute("disabled").ok());
            check!(self.export_button.remove_attribute("disabled").ok());
        }

        // update catalog tile usage counters
        let tiles: Vec<TileId> = self.map.tiles().iter()
            .map(|tile| tile.id())
            .collect();
        nuts::publish(UpdateTileUsageEvent { tiles });

        // remember map status
        nuts::send_to::<MapController, _>(SaveSettingsEvent);
     }

    pub fn update_title(&mut self, title: &str) {
        if title != self.map.title() {
            self.map.set_title(title);
            self.update_map();
        }
    }

    pub fn update_author(&mut self, author: &str) {
        if author != self.map.author() {
            self.map.set_author(author);
            nuts::send_to::<MapController, _>(SaveSettingsEvent);
        }
    }

    pub fn update_background_grid(&mut self, visible: bool) {
        if visible != !self.grid.hidden() {
            debug!("update background grid: {:?}", visible);
            self.grid.set_hidden(!visible);
            nuts::send_to::<MapController, _>(SaveSettingsEvent);
        }
    }

    pub fn update_tile_labels(&mut self, visible: bool) {
        if visible != self.tile_labels_visible {
            debug!("update map tile labels: {:?}", visible);
            self.tile_labels_visible = visible;
            self.update_map();
        }
    }

    pub fn toggle_tile_labels(&mut self, inverted: bool) {
        let visible = self.tile_labels_visible ^ inverted;
        info!("toggle map tile labels: {:?}", visible);
        self.labels.set_hidden(!visible);
    }

    pub fn clear_selected(&mut self) {
        self.selected.set_pos(&self.layout, None);
        self.selected.set_draggable(false);
        self.selected_menu.set_hidden(true);
    }

    pub fn update_selected_tile(&mut self, pos: Point) {
        let pos = Coordinate::from_pixel_rounded(&self.layout,
            self.canvas_viewbox.top_left() + pos);
        self.inner_update_selected_tile(pos);
        nuts::send_to::<MapController, _>(SaveSettingsEvent);
    }

    fn inner_update_selected_tile(&mut self, pos: Coordinate) {
        debug!("update selected tile: {:?}", pos);

        let is_tile = self.map.get(pos).is_some();
        if self.selected.pos() != Some(pos) || is_tile {
            self.selected.set_pos(&self.layout, Some(pos));
        } else {
            self.selected.set_pos(&self.layout, None);
        }
        self.selected.set_draggable(is_tile);
        self.selected_menu.set_pos(&self.layout, pos);
        self.selected_menu.set_hidden(!is_tile);

        // update active tile append position
        self.map.set_active_pos(pos);
        if let Some(active_pos) = self.map.active_pos() {
            self.active.update(&self.layout, active_pos, self.map.active_dir());
            self.active.set_hidden(false);
        } else {
            self.active.set_hidden(true);
        }
    }

    pub fn add_selected_tile_token(&mut self, token: PlacedToken) {
        if let Some(pos) = self.selected.pos() {
            self.map.add_tile_token(pos, token);
            self.update_map();
        }
    }

    pub fn update_selected_tile_tokens(&mut self, tokens: &[PlacedToken]) {
        if let Some(pos) = self.selected.pos() {
            self.map.update_tile_tokens(pos, tokens);
            self.update_map();
        }
    }

    pub fn rotate_selected_tile_left(&mut self) {
        let tile = self.selected.pos()
            .and_then(|pos| self.map.get(pos).cloned());
        if let Some(tile) = tile {
            self.map.insert_with_tokens(tile.id(), tile.pos, tile.dir.rotated_left(), tile.tokens);
            self.update_map();
        }
    }

    pub fn rotate_selected_tile_right(&mut self) {
        let tile = self.selected.pos()
            .and_then(|pos| self.map.get(pos).cloned());
        if let Some(tile) = tile {
            self.map.insert_with_tokens(tile.id(), tile.pos, tile.dir.rotated_right(), tile.tokens);
            self.update_map();
        }
    }

    pub fn remove_selected_tile(&mut self) {
        if let Some(pos) = self.selected.pos() {
            self.map.remove(pos);
            self.update_map();
        }
        self.selected.set_draggable(false);
        self.selected_menu.set_hidden(true);
    }

    pub fn drag_tile_begin(&mut self, pos: Point) {
        let pos = Coordinate::from_pixel_rounded(&self.layout,
            self.canvas_viewbox.top_left() + pos);
        if self.selected.pos() != Some(pos) {
            return;
        }
        if let Some(tile) = self.map.get(pos) {
            info!("drag tile begin: {:?}", tile);
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

    pub fn drag_tile_move(&mut self, pos: Point) {
        if let Some(ref dragged) = self.dragged {
            let pos = self.canvas_viewbox.top_left() + pos;
            debug!("drag tile move: {:?}", pos);
            dragged.set_pos(pos);
            // make dragged element visible on first mouse move
            dragged.set_hidden(false);
        }
        // hide menu during drag operation
        self.selected_menu.set_hidden(true);
    }

    pub fn drag_tile_end(&mut self, pos: Point, added_tile: Option<TileId>) {
        if let Some(ref dragged) = self.dragged {
            let pos = Coordinate::from_pixel_rounded(&self.layout,
                self.canvas_viewbox.top_left() + pos);
            let tile = dragged.tile();
            info!("drag tile end: {:?} -> {:?}", tile, pos);
            check!(self.canvas.remove_child(dragged.as_ref()).ok());
            if pos != tile.pos {
                self.map.remove(tile.pos);
                self.map.insert_with_tokens(tile.id(), pos, tile.dir, tile.tokens.clone());
                self.update_map();
            }
        }
        self.dragged = None;

        if let Some(tile) = added_tile {
            let pos = Coordinate::from_pixel_rounded(&self.layout,
                self.canvas_viewbox.top_left() + pos);
            info!("drag tile end: {:?} -> {:?}", tile, pos);
            self.map.append(tile, Some(pos), None);
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

    pub fn drag_tile_cancel(&mut self) {
        if let Some(ref dragged) = self.dragged {
            let tile = dragged.tile();
            info!("drag tile cancel: {:?}", tile);
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

    pub fn update_connection_hint(&mut self, hint: Option<ConnectionHint>) {
        self.active.set_hint(hint);
    }
}

impl Drop for MapView {
    fn drop(&mut self) {
        let document = check!(self.canvas.owner_document());
        let _ = document.remove_event_listener_with_callback("keydown",
            self.keychange_cb.as_ref().unchecked_ref());
        let _ = document.remove_event_listener_with_callback("keyup",
            self.keychange_cb.as_ref().unchecked_ref());
        let _ = self.canvas.remove_event_listener_with_callback("mousemove",
            self.dragged_mousemove_cb.as_ref().unchecked_ref());
        let _ = self.canvas.remove_event_listener_with_callback("mouseup",
            self.dragged_mouseup_cb.as_ref().unchecked_ref());
        let _ = self.canvas.remove_event_listener_with_callback("mouseleave",
            self.dragged_mouseleave_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------
