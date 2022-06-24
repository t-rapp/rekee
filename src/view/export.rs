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
use crate::map::{PlacedTile, Map};
use super::*;

//----------------------------------------------------------------------------

const PADDING: i32 = 2;
const TITLE_HEIGHT: i32 = 20;

fn draw_tile_image(context: &web_sys::CanvasRenderingContext2d, image: &HtmlImageElement,
    pos: Point, size: Point, angle: f32) -> Result<()>
{
    let pos_x = f64::from(pos.x());
    let pos_y = f64::from(pos.y());
    let size_x = 2.0 * f64::from(size.x());
    let size_y = 2.0 * f64::from(size.y());
    let angle = f64::from(angle * std::f32::consts::PI / 180.0);
    let scale = f64::min(
        size_x / image.natural_width() as f64,
        size_y / image.natural_height() as f64
    );
    let width = scale * image.natural_width() as f64;
    let height = scale * image.natural_height() as f64;

    context.translate(pos_x, pos_y)?;
    context.rotate(angle)?;
    context.translate(-pos_x, -pos_y)?;
    context.draw_image_with_html_image_element_and_dw_and_dh(image,
        pos_x - width / 2.0, pos_y - height / 2.0, width, height)?;
    context.set_transform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0)?;

    Ok(())
}

fn draw_missing_image(context: &web_sys::CanvasRenderingContext2d, pos: Point, size: Point) -> Result<()> {
    let pos_x = f64::from(pos.x());
    let pos_y = f64::from(pos.y());
    let height = 1.6 * size.y();

    context.set_font(&format!("bold {:.0}px sans-serif", height));
    context.set_text_align("center");
    context.set_text_baseline("middle");
    context.set_fill_style(&JsValue::from_str("#EEE"));
    context.fill_text("?", pos_x, pos_y)?;

    Ok(())
}

fn draw_tile_label(context: &web_sys::CanvasRenderingContext2d, text: &str, pos: Point) -> Result<()> {
    let pos_x = f64::from(pos.x());
    let pos_y = f64::from(pos.y());

    context.set_font("bold 14px sans-serif");
    context.set_text_align("center");
    context.set_text_baseline("middle");
    context.set_line_width(2.0);
    context.set_stroke_style(&JsValue::from_str("#FFF"));
    context.stroke_text(text, pos_x, pos_y)?;
    context.set_fill_style(&JsValue::from_str("#444"));
    context.fill_text(text, pos_x, pos_y)?;

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
    fn new(context: &web_sys::CanvasRenderingContext2d, layout: &Layout, tile: PlacedTile, label_visible: bool) -> Result<Self> {
        let pos = tile.pos.to_pixel(layout);
        let size = layout.size();
        let angle = tile.dir.to_angle(layout);
        let image = HtmlImageElement::new()?;

        let load_cb = Closure::wrap(Box::new({
            let tile = tile.clone();
            let image = image.clone();
            let context = context.clone();
            move |_event: web_sys::Event| {
                debug!("drawing tile image {:?}", &tile);
                context.save();
                check!(draw_tile_image(&context, &image, pos, size, angle).ok());
                if label_visible {
                    check!(draw_tile_label(&context, &tile.id().base().to_string(), pos).ok());
                }
                context.restore();
                nuts::send_to::<ExportController, _>(DrawExportTileDoneEvent { tile: tile.clone() });
            }
        }) as Box<dyn Fn(_)>);
        image.add_event_listener_with_callback("load", load_cb.as_ref().unchecked_ref())?;

        let error_cb = Closure::wrap(Box::new({
            let tile = tile.clone();
            let context = context.clone();
            move |_event: web_sys::Event| {
                debug!("loading of tile image {:?} failed", &tile);
                context.save();
                check!(draw_missing_image(&context, pos, size).ok());
                if label_visible {
                    check!(draw_tile_label(&context, &tile.id().base().to_string(), pos).ok());
                }
                context.restore();
                nuts::send_to::<ExportController, _>(DrawExportTileDoneEvent { tile: tile.clone() });
            }
        }) as Box<dyn Fn(_)>);
        image.add_event_listener_with_callback("error", error_cb.as_ref().unchecked_ref())?;

        // start loading the tile image
        let url = format!("tiles/thumb-{}.png", tile.id());
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

pub struct ExportView {
    layout: Layout,
    images: Vec<TileImage>,
    canvas: HtmlCanvasElement,
    anchor: HtmlElement,
}

impl ExportView {
    pub fn new(parent: Element, layout: &Layout) -> Result<Self> {
        let layout = layout.clone();
        let images = Vec::new();
        let document = parent.owner_document().unwrap();

        let canvas = document.create_element("canvas")?
            .dyn_into::<HtmlCanvasElement>()?;
        parent.append_child(&canvas)?;

        let anchor = document.create_element("a")?
            .dyn_into::<HtmlElement>()?;
        anchor.set_hidden(true);
        parent.append_child(&anchor)?;

        Ok(ExportView { layout, images, canvas, anchor })
    }

    pub fn draw_export_image(&mut self, map: &Map, tile_labels_visible: bool) {
        debug!("start export of map image with {} tiles", map.tiles().len());

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
        let layout = Layout::new(self.layout.orientation(), self.layout.size(), origin);
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
        self.images.clear();
        for tile in map.tiles() {
            let image = check!(TileImage::new(&context, &layout, tile.clone(), tile_labels_visible).ok());
            self.images.push(image);
        }
    }

    pub fn draw_export_tile_done(&mut self, tile: &PlacedTile) {
        debug!("export of tile image at {} is finished", tile.pos);
        // remove tile from todo list
        self.images.retain(|img| img.tile != *tile);

        if self.images.is_empty() {
            debug!("export of all tile images is completed");
            let url = check!(self.canvas.to_data_url_with_type("image/png").ok());
            check!(self.anchor.set_attribute("href", &url).ok());
            self.anchor.click();
        }
    }
}

//----------------------------------------------------------------------------
