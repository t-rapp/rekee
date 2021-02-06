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

use log::{debug, trace};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, HtmlCanvasElement, HtmlImageElement, HtmlElement};

use crate::check;
use crate::controller::*;
use crate::import;
use super::*;

//----------------------------------------------------------------------------

const PADDING: f32 = 2.0;
const TITLE_HEIGHT: f32 = 24.0;

struct TileImage {
    image: HtmlImageElement,
    tile: PlacedTile,
    load_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl TileImage {
    fn new(context: &web_sys::CanvasRenderingContext2d, layout: &Layout, tile: PlacedTile) -> Result<Self> {
        use std::f32::consts::PI;
        let pos = tile.pos.to_pixel(layout);
        let pos_x = f64::from(pos.x());
        let pos_y = f64::from(pos.y());
        let size = layout.size();
        let size_x = 2.0 * f64::from(size.x());
        let size_y = 2.0 * f64::from(size.y());
        let angle = tile.dir.to_angle(layout);
        let angle = f64::from(angle * PI / 180.0);
        let image = HtmlImageElement::new()?;

        let load_cb = Closure::wrap(Box::new({
            let tile = tile.clone();
            let image = image.clone();
            let context = context.clone();
            move |_event: web_sys::Event| {
                debug!("draw tile image {:?}", tile);
                let scale = f64::min(
                    size_x / image.natural_width() as f64,
                    size_y / image.natural_height() as f64
                );
                let width = scale * image.natural_width() as f64;
                let height = scale * image.natural_height() as f64;
                trace!("x={:.1}, y={:.1}, width={:.1}, height={:.1}", pos_x, pos_y, width, height);
                context.save();

                // draw tile image
                check!(context.translate(pos_x, pos_y).ok());
                check!(context.rotate(angle).ok());
                check!(context.translate(-pos_x, -pos_y).ok());
                check!(context.draw_image_with_html_image_element_and_dw_and_dh(&image, pos_x - width / 2.0, pos_y - height / 2.0, width, height).ok());
                check!(context.set_transform(1.0, 0.0, 0.0, 1.0, 0.0, 0.0).ok());

                // draw tile label
                context.set_font("bold 14px sans-serif");
                context.set_text_align("center");
                context.set_text_baseline("middle");
                context.set_line_width(2.0);
                context.set_stroke_style(&JsValue::from_str("#FFF"));
                check!(context.stroke_text(&tile.id().base().to_string(), pos_x, pos_y).ok());
                context.set_fill_style(&JsValue::from_str("#444"));
                check!(context.fill_text(&tile.id().base().to_string(), pos_x, pos_y).ok());

                context.restore();
                nuts::publish(DrawExportTileDoneEvent { tile: tile.clone() });
            }
        }) as Box<dyn Fn(_)>);
        image.add_event_listener_with_callback("load", load_cb.as_ref().unchecked_ref())?;

        // start loading the tile image
        let url = format!("tiles/thumb-{}.png", tile.id());
        image.set_attribute("src", &url)?;

        Ok(TileImage { image, tile, load_cb })
    }
}

impl Drop for TileImage {
    fn drop(&mut self) {
        let _ = self.image.remove_event_listener_with_callback("load",
            self.load_cb.as_ref().unchecked_ref());
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

    pub fn draw_export_image(&mut self, map: &Map) {
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
        trace!("map area: {:?}, layout origin: {:?}", map_area, self.layout.origin());

        let width = (map_area.width + 2.0 * PADDING) as i32;
        let height = (map_area.height + TITLE_HEIGHT + 3.0 * PADDING) as i32;
        check!(self.canvas.set_attribute("width", &width.to_string()).ok());
        check!(self.canvas.set_attribute("height", &height.to_string()).ok());

        let origin = self.layout.origin() - Point(map_area.left, map_area.top) +
            Point(PADDING, TITLE_HEIGHT + 2.0 * PADDING);
        let layout = Layout::new(self.layout.orientation(), self.layout.size(), origin);
        let context = check!(self.canvas.get_context("2d").ok().flatten()
            .and_then(|obj| obj.dyn_into::<web_sys::CanvasRenderingContext2d>().ok()));

        context.save();
        // draw background color
        context.set_fill_style(&JsValue::from_str("#FFF"));
        context.fill_rect(0.0, 0.0, f64::from(width), f64::from(height));
        // draw map title
        context.set_font("normal 24px sans-serif");
        context.set_text_align("left");
        context.set_text_baseline("top");
        context.set_fill_style(&JsValue::from_str("#222"));
        check!(context.fill_text(map.title(), f64::from(PADDING), f64::from(PADDING)).ok());
        context.restore();

        // draw each tile image asynchronously
        for tile in map.tiles() {
            let image = check!(TileImage::new(&context, &layout, tile.clone()).ok());
            self.images.push(image);
        }
    }

    pub fn draw_export_tile_done(&mut self, tile: &PlacedTile) {
        debug!("export of tile image at {} is finished", tile.pos);
        // remove tile from todo list
        self.images.retain(|img| img.tile != *tile);

        if self.images.len() == 0 {
            debug!("export of all tile images is completed");
            let url = check!(self.canvas.to_data_url_with_type("image/png").ok());
            check!(self.anchor.set_attribute("href", &url).ok());
            self.anchor.click();
        }
    }
}

//----------------------------------------------------------------------------
