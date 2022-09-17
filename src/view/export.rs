//----------------------------------------------------------------------------
//! Supports export of maps as an image.
//!
//! Tile images are draw asynchronously as they need to be retrieved and decoded
//! by the browser. The current implementation is based on posting global
//! messages to the [`ExportView`] instance for each tile drawn. When the last
//! tile is finished the client-side image download will be triggered.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, HtmlCanvasElement, HtmlImageElement, HtmlElement};

use crate::check;
use crate::controller::*;
use crate::import;
use crate::map::{PlacedTile, PlacedToken, Map};
use super::*;

//----------------------------------------------------------------------------

const PADDING: i32 = 2;
const TITLE_HEIGHT: i32 = 20;

fn draw_tile_image(context: &web_sys::CanvasRenderingContext2d, image: &HtmlImageElement,
    pos: Point, size: Point, angle: f32) -> Result<()>
{
    let center = Point(0.5 * size.x(), 0.5 * size.y());
    draw_token_image(context, image, pos, size, center, angle)
}

fn draw_token_image(context: &web_sys::CanvasRenderingContext2d, image: &HtmlImageElement,
    pos: Point, size: Point, center: Point, angle: f32) -> Result<()>
{
    let pos_x = f64::from(pos.x());
    let pos_y = f64::from(pos.y());
    let size_x = f64::from(size.x());
    let size_y = f64::from(size.y());
    let center_x = f64::from(center.x());
    let center_y = f64::from(center.y());
    let angle = f64::from(angle.to_radians());

    context.save();
    context.translate(pos_x, pos_y)?;
    context.rotate(angle)?;
    context.translate(-pos_x, -pos_y)?;
    context.draw_image_with_html_image_element_and_dw_and_dh(image,
        pos_x - center_x, pos_y - center_y, size_x, size_y)?;
    // reset current transformation matrix to the identity matrix
    context.set_transform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)?;
    context.restore();

    Ok(())
}

fn draw_missing_image(context: &web_sys::CanvasRenderingContext2d, pos: Point, size: Point) -> Result<()> {
    let pos_x = f64::from(pos.x());
    let pos_y = f64::from(pos.y());
    let height = 0.8 * f32::max(size.x(), size.y());

    context.save();
    context.set_font(&format!("bold {:.0}px sans-serif", height));
    context.set_text_align("center");
    context.set_text_baseline("middle");
    context.set_fill_style(&JsValue::from_str("#EEE"));
    context.fill_text("?", pos_x, pos_y)?;
    context.restore();

    Ok(())
}

fn draw_tile_label(context: &web_sys::CanvasRenderingContext2d, text: &str, pos: Point) -> Result<()> {
    let pos_x = f64::from(pos.x());
    let pos_y = f64::from(pos.y());

    context.save();
    context.set_font("bold 14px sans-serif");
    context.set_text_align("center");
    context.set_text_baseline("middle");
    context.set_line_width(2.0);
    context.set_stroke_style(&JsValue::from_str("#FFF"));
    context.stroke_text(text, pos_x, pos_y)?;
    context.set_fill_style(&JsValue::from_str("#444"));
    context.fill_text(text, pos_x, pos_y)?;
    context.restore();

    Ok(())
}

//----------------------------------------------------------------------------

struct TileImage {
    image: HtmlImageElement,
    tile: PlacedTile,
    load_cb: Closure<dyn Fn(web_sys::Event)>,
    error_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl TileImage {
    fn new(context: &web_sys::CanvasRenderingContext2d, layout: &Layout, tile: PlacedTile) -> Result<Self> {
        let pos = tile.pos.to_pixel(layout);
        let size = tile_image_size(layout);
        let angle = layout.direction_to_angle(tile.dir);
        let image = HtmlImageElement::new()?;

        let load_cb = Closure::wrap(Box::new({
            let tile = tile.clone();
            let image = image.clone();
            let context = context.clone();
            move |_event: web_sys::Event| {
                debug!("drawing tile image {} at {}", &tile.id(), &tile.pos);
                check!(draw_tile_image(&context, &image, pos, size, angle).ok());
                nuts::send_to::<ExportController, _>(DrawExportTileDoneEvent { tile: tile.clone() });
            }
        }) as Box<dyn Fn(_)>);
        image.add_event_listener_with_callback("load", load_cb.as_ref().unchecked_ref())?;

        let error_cb = Closure::wrap(Box::new({
            let tile = tile.clone();
            let context = context.clone();
            move |_event: web_sys::Event| {
                debug!("loading of tile image {} failed", &tile.id());
                check!(draw_missing_image(&context, pos, size).ok());
                nuts::send_to::<ExportController, _>(DrawExportTileDoneEvent { tile: tile.clone() });
            }
        }) as Box<dyn Fn(_)>);
        image.add_event_listener_with_callback("error", error_cb.as_ref().unchecked_ref())?;

        // start loading the tile image
        let url = format!("tiles/thumbs/tile-{:x}.webp", tile.id());
        image.set_attribute("src", &url)?;

        Ok(TileImage { image, tile, load_cb, error_cb })
    }
}

impl Drop for TileImage {
    fn drop(&mut self) {
        let _ = self.image.remove_event_listener_with_callback("load",
            self.load_cb.as_ref().unchecked_ref());
        let _ = self.image.remove_event_listener_with_callback("error",
            self.error_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------

struct TokenImage {
    image: HtmlImageElement,
    tile: PlacedTile,
    token: PlacedToken,
    load_cb: Closure<dyn Fn(web_sys::Event)>,
    error_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl TokenImage {
    fn new(context: &web_sys::CanvasRenderingContext2d, layout: &Layout, tile: PlacedTile, token: PlacedToken) -> Result<Self> {
        let pos = (FloatCoordinate::from(tile.pos) + token.pos.rotate(tile.dir))
            .to_pixel(layout);
        let size = token_image_size(layout, &token);
        let center = token_image_center(layout, &token);
        let angle = layout.direction_to_angle(FloatDirection::from(tile.dir) + token.dir);
        let image = HtmlImageElement::new()?;

        let load_cb = Closure::wrap(Box::new({
            let tile = tile.clone();
            let token = token.clone();
            let image = image.clone();
            let context = context.clone();
            move |_event: web_sys::Event| {
                debug!("drawing token image {} at tile {}", &token.id, &tile.pos);
                check!(draw_token_image(&context, &image, pos, size, center, angle).ok());
                nuts::send_to::<ExportController, _>(DrawExportTokenDoneEvent { tile: tile.clone(), token: token.clone() });
            }
        }) as Box<dyn Fn(_)>);
        image.add_event_listener_with_callback("load", load_cb.as_ref().unchecked_ref())?;

        let error_cb = Closure::wrap(Box::new({
            let tile = tile.clone();
            let token = token.clone();
            let context = context.clone();
            move |_event: web_sys::Event| {
                debug!("loading of token image {} failed", &token.id);
                check!(draw_missing_image(&context, pos, size).ok());
                nuts::send_to::<ExportController, _>(DrawExportTokenDoneEvent { tile: tile.clone(), token: token.clone() });
            }
        }) as Box<dyn Fn(_)>);
        image.add_event_listener_with_callback("error", error_cb.as_ref().unchecked_ref())?;

        // start loading the tile image
        let url = format!("tokens/{:x}.png", token.id);
        image.set_attribute("src", &url)?;

        Ok(TokenImage { image, tile, token, load_cb, error_cb })
    }
}

impl Drop for TokenImage {
    fn drop(&mut self) {
        let _ = self.image.remove_event_listener_with_callback("load",
            self.load_cb.as_ref().unchecked_ref());
        let _ = self.image.remove_event_listener_with_callback("error",
            self.error_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------

pub struct ExportView {
    layout: Layout,
    map: Map,
    tile_labels_visible: bool,
    tile_images: Vec<TileImage>,
    token_images: Vec<TokenImage>,
    canvas: HtmlCanvasElement,
    anchor: HtmlElement,
}

impl ExportView {
    pub fn new(parent: Element, layout: &Layout) -> Result<Self> {
        let layout = layout.clone();
        let map = Map::new();
        let tile_labels_visible = true;
        let tile_images = Vec::new();
        let token_images = Vec::new();
        let document = parent.owner_document().unwrap();

        let canvas = document.create_element("canvas")?
            .dyn_into::<HtmlCanvasElement>()?;
        parent.append_child(&canvas)?;

        let anchor = document.create_element("a")?
            .dyn_into::<HtmlElement>()?;
        anchor.set_hidden(true);
        parent.append_child(&anchor)?;

        Ok(ExportView { layout, map, tile_labels_visible, tile_images, token_images, canvas, anchor })
    }

    pub fn draw_export_image(&mut self, map: &Map, tile_labels_visible: bool) {
        debug!("start export of map image with {} tiles", map.tiles().len());
        self.map = map.clone();
        self.tile_labels_visible = tile_labels_visible;

        // update download filename attribute
        let mut file_name = import::build_file_name(map.title());
        file_name.push_str(".png");
        check!(self.anchor.set_attribute("download", &file_name).ok());

        // calculate rectangular map area that is covered with tiles
        let mut map_area = Rect::new(f32::NAN, f32::NAN, 0.0, 0.0);
        for tile in map.tiles() {
            map_area = map_area.union(&self.layout.hexagon_rect(tile.pos));
        }
        map_area.left = map_area.left.floor();
        map_area.top = map_area.top.floor();
        map_area.width = map_area.width.ceil();
        map_area.height = map_area.height.ceil();
        debug!("map area: {:?}, layout origin: {:?}", map_area, self.layout.origin());

        let width = map_area.width as i32 + 2 * PADDING;
        let mut height = map_area.height as i32 + 2 * PADDING;
        let has_title = !map.title().is_empty();
        if has_title {
            height = height + TITLE_HEIGHT + PADDING;
        }
        check!(self.canvas.set_attribute("width", &width.to_string()).ok());
        check!(self.canvas.set_attribute("height", &height.to_string()).ok());

        let mut origin = self.layout.origin() - Point(map_area.left, map_area.top) +
            Point(PADDING as f32, PADDING as f32);
        if has_title {
            origin = origin + Point(0.0, (TITLE_HEIGHT + PADDING) as f32);
        }
        let layout = self.layout.with_origin(origin);
        let context = check!(self.canvas.get_context("2d").ok().flatten()
            .and_then(|obj| obj.dyn_into::<web_sys::CanvasRenderingContext2d>().ok()));

        context.save();
        // draw background color
        context.set_fill_style(&JsValue::from_str("#FFF"));
        context.fill_rect(0.0, 0.0, f64::from(width), f64::from(height));
        // draw map title
        if has_title {
            context.set_font(&format!("normal {}px sans-serif", TITLE_HEIGHT));
            context.set_text_align("left");
            context.set_text_baseline("top");
            context.set_fill_style(&JsValue::from_str("#222"));
            check!(context.fill_text(map.title(), f64::from(PADDING), f64::from(PADDING)).ok());
        }
        context.restore();

        // draw each tile image asynchronously
        self.tile_images.clear();
        for tile in map.tiles() {
            let image = check!(TileImage::new(&context, &layout, tile.clone()).ok());
            self.tile_images.push(image);
        }

        self.layout = layout;
    }

    pub fn draw_export_tile_done(&mut self, tile: &PlacedTile) {
        debug!("export of tile image at {} is finished", tile.pos);
        // remove tile from todo list
        self.tile_images.retain(|img| img.tile != *tile);

        if self.tile_images.is_empty() {
            let context = check!(self.canvas.get_context("2d").ok().flatten()
                .and_then(|obj| obj.dyn_into::<web_sys::CanvasRenderingContext2d>().ok()));

            // draw each token image asynchronously
            self.token_images.clear();
            for tile in self.map.tiles() {
                debug!("adding tokens for tile {:?}", &tile);
                for token in &tile.tokens {
                    let image = check!(TokenImage::new(&context, &self.layout, tile.clone(), token.clone()).ok());
                    self.token_images.push(image);
                }
            }

            if self.token_images.is_empty() {
                self.draw_export_image_completed();
            }
        }
    }

    pub fn draw_export_token_done(&mut self, tile: &PlacedTile, token: &PlacedToken) {
        debug!("export of token image {} at {} is finished", token.id, tile.pos);
        // remove token image from todo list
        self.token_images.retain(|img| img.tile != *tile || img.token != *token);

        if self.token_images.is_empty() {
            self.draw_export_image_completed();
        }
    }

    fn draw_export_image_completed(&mut self) {
        // finally draw tile labels on top of everything other
        if self.tile_labels_visible {
            debug!("drawing tile labels");
            let context = check!(self.canvas.get_context("2d").ok().flatten()
                .and_then(|obj| obj.dyn_into::<web_sys::CanvasRenderingContext2d>().ok()));
            for tile in self.map.tiles() {
                let pos = tile.pos.to_pixel(&self.layout);
                check!(draw_tile_label(&context, &tile.id().base().to_string(), pos).ok());
            }
        }

        // trigger download of canvas image data
        debug!("export of all tile images is completed");
        let url = check!(self.canvas.to_data_url_with_type("image/png").ok());
        check!(self.anchor.set_attribute("href", &url).ok());
        self.anchor.click();
    }
}

//----------------------------------------------------------------------------
