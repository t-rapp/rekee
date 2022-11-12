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

use std::fmt;
use std::str::FromStr;

use serde::{Serialize, Deserialize};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, HtmlCanvasElement, HtmlImageElement, HtmlElement};

use crate::check;
use crate::controller::*;
use crate::controller::export::*;
use crate::import;
use crate::map::{PlacedTile, PlacedToken, Map};
use super::*;

//----------------------------------------------------------------------------

const PADDING: i32 = 2;

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

fn draw_tile_label(context: &web_sys::CanvasRenderingContext2d, text: &str, pos: Point, height: i32) -> Result<()> {
    let pos_x = f64::from(pos.x());
    let pos_y = f64::from(pos.y());

    context.save();
    context.set_font(&format!("bold {}px sans-serif", height));
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "kebab-case", try_from = "&str")]
pub enum ExportScale {
    Small,
    Medium,
    Large,
    ExtraLarge,
}

impl ExportScale {
    const fn title_height(&self) -> i32 {
        match self {
            ExportScale::Small => 20,
            ExportScale::Medium => 24,
            ExportScale::Large => 30,
            ExportScale::ExtraLarge => 40,
        }
    }

    const fn tile_label_height(&self) -> i32 {
        match self {
            ExportScale::Small => 14,
            ExportScale::Medium => 16,
            ExportScale::Large => 20,
            ExportScale::ExtraLarge => 26,
        }
    }
}

impl Default for ExportScale {
    fn default() -> Self {
        ExportScale::Medium
    }
}

impl fmt::Display for ExportScale {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExportScale::Small =>
                write!(fmt, "Small")?,
            ExportScale::Medium =>
                write!(fmt, "Medium")?,
            ExportScale::Large =>
                write!(fmt, "Large")?,
            ExportScale::ExtraLarge =>
                write!(fmt, "Extra Large")?,
        }
        Ok(())
    }
}

impl FromStr for ExportScale {
    type Err = ParseExportScaleError;

    fn from_str(val: &str) -> std::result::Result<Self, Self::Err> {
        // match strings from both trait implementations, std::fmt::Display and serde::Serialize
        let mut s = val.replace(char::is_whitespace, "-");
        s.make_ascii_lowercase();

        match s.as_ref() {
            "small" =>
                Ok(ExportScale::Small),
            "medium" =>
                Ok(ExportScale::Medium),
            "large" =>
                Ok(ExportScale::Large),
            "extra-large" =>
                Ok(ExportScale::ExtraLarge),
            _ =>
                Err(ParseExportScaleError::Unknown(val.to_string())),
        }
    }
}

impl From<ExportScale> for f32 {
    fn from(val: ExportScale) -> f32 {
        match val {
            ExportScale::Small =>
                1.0,
            ExportScale::Medium =>
                1.2,
            ExportScale::Large =>
                1.5,
            ExportScale::ExtraLarge =>
                2.0,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseExportScaleError {
    Unknown(String),
}

impl fmt::Display for ParseExportScaleError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseExportScaleError::Unknown(val) =>
                write!(fmt, "Unknown export scale value \"{}\"", val),
        }
    }
}

impl std::convert::TryFrom<&str> for ExportScale {
    type Error = ParseExportScaleError;

    fn try_from(value: &str) -> std::result::Result<Self, Self::Error> {
        ExportScale::from_str(value)
    }
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
        let size = token_image_size(layout, token.id);
        let center = token_image_center(layout, token.id);
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
        let url = format!("tokens/thumbs/{:x}.webp", token.id);
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

#[derive(Default, Serialize, Deserialize)]
#[serde(default)]
pub struct ExportSettings {
    pub export_scale: Option<ExportScale>,
}

//----------------------------------------------------------------------------

pub struct ExportView {
    /// Layout value as set during initialization
    base_layout: Layout,
    /// Layout for the currently active export
    export_layout: Layout,
    export_scale: Option<ExportScale>,
    map: Map,
    tile_labels_visible: bool,
    tile_images: Vec<TileImage>,
    token_images: Vec<TokenImage>,
    canvas: HtmlCanvasElement,
    anchor: HtmlElement,
}

impl ExportView {
    pub fn new(parent: Element, layout: &Layout) -> Result<Self> {
        let base_layout = layout.clone();
        let export_layout = layout.clone();
        let export_scale = None;
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

        Ok(ExportView {
            base_layout, export_layout, export_scale, map, tile_labels_visible,
            tile_images, token_images, canvas, anchor
        })
    }

    pub fn load_settings(&mut self, settings: &ExportSettings) {
        self.export_scale = settings.export_scale;
    }

    pub fn save_settings(&mut self) -> ExportSettings {
        let export_scale = self.export_scale;
        ExportSettings { export_scale }
    }

    pub fn update_export_scale(&mut self, scale: Option<ExportScale>) {
        self.export_scale = scale;
        nuts::send_to::<ExportController, _>(SaveSettingsEvent {});
    }

    pub fn draw_export_image(&mut self, map: &Map, tile_labels_visible: bool) {
        debug!("start export of map image with {} tiles and {} scale", map.tiles().len(),
            self.export_scale.map(|val| val.to_string()).unwrap_or_else(|| String::from("default")));
        self.map = map.clone();
        self.tile_labels_visible = tile_labels_visible;

        // update download filename attribute
        let mut file_name = import::build_file_name(map.title());
        file_name.push_str(".png");
        check!(self.anchor.set_attribute("download", &file_name).ok());

        let export_scale = self.export_scale.unwrap_or_default();
        let export_layout = self.base_layout
            .with_size(self.base_layout.size() * f32::from(export_scale));

        // calculate rectangular map area that is covered with tiles
        let mut map_area = Rect::new(f32::NAN, f32::NAN, 0.0, 0.0);
        for tile in map.tiles() {
            map_area = map_area.union(&export_layout.hexagon_rect(tile.pos));
        }
        map_area.left = map_area.left.floor();
        map_area.top = map_area.top.floor();
        map_area.width = map_area.width.ceil();
        map_area.height = map_area.height.ceil();
        debug!("map area: {:?}, layout origin: {:?}", map_area, export_layout.origin());

        let width = map_area.width as i32 + 2 * PADDING;
        let mut height = map_area.height as i32 + 2 * PADDING;
        let has_title = !map.title().is_empty();
        if has_title {
            height = height + export_scale.title_height() + PADDING;
        }
        check!(self.canvas.set_attribute("width", &width.to_string()).ok());
        check!(self.canvas.set_attribute("height", &height.to_string()).ok());

        let mut origin = export_layout.origin() - Point(map_area.left, map_area.top) +
            Point(PADDING as f32, PADDING as f32);
        if has_title {
            origin = origin + Point(0.0, (export_scale.title_height() + PADDING) as f32);
        }
        let export_layout = export_layout.with_origin(origin);

        let context = check!(self.canvas.get_context("2d").ok().flatten()
            .and_then(|obj| obj.dyn_into::<web_sys::CanvasRenderingContext2d>().ok()));

        context.save();
        // draw background color
        context.set_fill_style(&JsValue::from_str("#FFF"));
        context.fill_rect(0.0, 0.0, f64::from(width), f64::from(height));
        // draw map title
        if has_title {
            context.set_font(&format!("normal {}px sans-serif", export_scale.title_height()));
            context.set_text_align("left");
            context.set_text_baseline("top");
            context.set_fill_style(&JsValue::from_str("#222"));
            check!(context.fill_text(map.title(), f64::from(PADDING), f64::from(PADDING)).ok());
        }
        context.restore();

        // draw each tile image asynchronously
        self.tile_images.clear();
        for tile in map.tiles() {
            let image = check!(TileImage::new(&context, &export_layout, tile.clone()).ok());
            self.tile_images.push(image);
        }

        self.export_layout = export_layout;
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
                    let image = check!(TokenImage::new(&context, &self.export_layout, tile.clone(), token.clone()).ok());
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
        let export_scale = self.export_scale.unwrap_or_default();

        // finally draw tile labels on top of everything other
        if self.tile_labels_visible {
            debug!("drawing tile labels");
            let context = check!(self.canvas.get_context("2d").ok().flatten()
                .and_then(|obj| obj.dyn_into::<web_sys::CanvasRenderingContext2d>().ok()));
            for tile in self.map.tiles() {
                let pos = tile.pos.to_pixel(&self.export_layout);
                check!(draw_tile_label(&context, &tile.id().base().to_string(), pos, export_scale.tile_label_height()).ok());
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn export_scale_to_str() {
        assert_eq!(ExportScale::Small.to_string(), "Small");
        assert_eq!(ExportScale::Medium.to_string(), "Medium");
        assert_eq!(ExportScale::Large.to_string(), "Large");
        assert_eq!(ExportScale::ExtraLarge.to_string(), "Extra Large");
    }

    #[test]
    fn export_scale_from_str() {
        assert_eq!("small".parse::<ExportScale>(), Ok(ExportScale::Small));
        assert_eq!("Medium".parse::<ExportScale>(), Ok(ExportScale::Medium));
        assert_eq!("LARGE".parse::<ExportScale>(), Ok(ExportScale::Large));
        assert_eq!("EXTRA LARGE".parse::<ExportScale>(), Ok(ExportScale::ExtraLarge));

        assert!("".parse::<ExportScale>().is_err());
        assert!("xl".parse::<ExportScale>().is_err());
    }

    #[test]
    fn export_scale_serde() {
        let text = serde_json::to_string(&ExportScale::Medium).unwrap();
        assert_eq!(text, r#""medium""#);

        let text = r#""extra-large""#;
        let scale: ExportScale = serde_json::from_str(text).unwrap();
        assert_eq!(scale, ExportScale::ExtraLarge);

        let text = r#""#;
        let result: std::result::Result<ExportScale, _> = serde_json::from_str(text);
        assert!(result.is_err());

        let text = r#""xl""#;
        let result: std::result::Result<ExportScale, _> = serde_json::from_str(text);
        assert!(result.is_err());
    }
}

//----------------------------------------------------------------------------
