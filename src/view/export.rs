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
use web_sys::{self, FontFace, HtmlCanvasElement, HtmlImageElement, HtmlElement};

use crate::check;
use crate::controller::SaveSettingsEvent;
use crate::controller::export::*;
use crate::hexagon::Rect;
use crate::import;
use crate::map::{PlacedTile, PlacedToken, Map};
use super::*;

//----------------------------------------------------------------------------

const PADDING: i32 = 4;

const BACKGROUND_COLOR: &str = "hsl(0, 0%, 100%)";
const MAP_TITLE_COLOR: &str = "hsl(12, 71%, 43%)";
const MAP_AUTHOR_COLOR: &str = "hsl(12, 56%, 67%)";
const MAP_PREPOSITION_COLOR: &str = "hsl(93, 27%, 63%)";
const MAP_BORDER_COLOR: &str = "hsl(93, 49%, 38%)";
const MISSING_IMAGE_COLOR: &str = "hsl(0, 0%, 90%)";
const TILE_LABEL_COLOR: &str = "hsl(0, 0%, 30%)";

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
    let height = (0.8 * f32::max(size.x(), size.y())).round() as i32;

    context.save();
    context.set_font(&format!("bold {}px Overpass, sans-serif", height));
    context.set_text_align("center");
    context.set_text_baseline("middle");
    context.set_fill_style(&JsValue::from_str(MISSING_IMAGE_COLOR));
    context.fill_text("?", pos_x, pos_y)?;
    context.restore();

    Ok(())
}

fn draw_tile_label(context: &web_sys::CanvasRenderingContext2d, text: &str, pos: Point, height: i32) -> Result<()> {
    let pos_x = f64::from(pos.x());
    let pos_y = f64::from(pos.y());

    context.save();
    context.set_font(&format!("bold {}px OverpassTnum, Overpass, sans-serif", height));
    context.set_text_align("center");
    context.set_text_baseline("middle");
    context.set_line_width(2.0);
    context.set_miter_limit(2.0);
    context.set_stroke_style(&JsValue::from_str(BACKGROUND_COLOR));
    context.stroke_text(text, pos_x, pos_y)?;
    context.set_fill_style(&JsValue::from_str(TILE_LABEL_COLOR));
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

    const fn author_height(&self) -> i32 {
        match self {
            ExportScale::Small => 14,
            ExportScale::Medium => 16,
            ExportScale::Large => 21,
            ExportScale::ExtraLarge => 28,
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
    pub header_visible: bool,
}

//----------------------------------------------------------------------------

pub struct ExportView {
    /// Layout value as set during initialization
    base_layout: Layout,
    /// Layout for the currently active export
    export_layout: Layout,
    export_scale: Option<ExportScale>,
    map: Map,
    header_visible: bool,
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
        let header_visible = true;
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

        let title_font = FontFace::new_with_str("Overpass",
            "url('overpass-bold.woff2') format('woff2')")?;
        title_font.set_style("normal");
        title_font.set_weight("bold");
        document.fonts().add(&title_font)?;
        let _ = title_font.load();

        // Not all browser + operating system combinations have support for enabling the tabular
        // number glyph variants by using FontFace::set_feature_settings(). If such support is
        // available it saves us from downloading an extra font file for tile labels, so we
        // auto-detect browser support.
        let font_load_cb = Closure::wrap(Box::new({
            let canvas = canvas.clone();
            move |font: JsValue| {

                let context = check!(canvas.get_context("2d").ok().flatten()
                    .and_then(|obj| obj.dyn_into::<web_sys::CanvasRenderingContext2d>().ok()));
                context.save();
                context.set_font("bold 16px OverpassTnum, sans-serif");
                let width_str0 = check!(context.measure_text("0000").ok()).width();
                let width_str1 = check!(context.measure_text("1111").ok()).width();
                debug!("label font width 0/1: {:.3}/{:.3}, ratio: {:.3}",
                    width_str0, width_str1, width_str0 / width_str1);
                context.restore();

                // compare widths of both numerical sample strings, trigger
                // load of a replacement font if they differ by more than 20%
                if (width_str0 - width_str1).abs() > 0.2 * width_str0 {
                    warn!("Detected missing support for font settings configuration in the Canvas API of this browser. Loading a replacement font instead.");

                    let document = canvas.owner_document().unwrap();
                    let font = check!(font.dyn_into::<FontFace>().ok());
                    document.fonts().delete(&font);

                    let label_font = check!(FontFace::new_with_str("OverpassTnum",
                        "url('overpass+tnum-bold.woff2') format('woff2')").ok());
                    label_font.set_style("normal");
                    label_font.set_weight("bold");
                    check!(document.fonts().add(&label_font).ok());
                    let _ = label_font.load();
                }
            }
        }) as Box<dyn FnMut(_)>);

        let label_font = FontFace::new_with_str("OverpassTnum",
            "url('overpass-bold.woff2') format('woff2')")?;
        label_font.set_style("normal");
        label_font.set_weight("bold");
        label_font.set_feature_settings("\"tnum\" on");
        document.fonts().add(&label_font)?;
        let _ = label_font.load()?
            .then(&font_load_cb);
        font_load_cb.forget();

        Ok(ExportView {
            base_layout, export_layout, export_scale, map, header_visible,
            tile_labels_visible, tile_images, token_images, canvas, anchor
        })
    }

    pub fn load_settings(&mut self, settings: &ExportSettings) {
        self.export_scale = settings.export_scale;
        self.header_visible = settings.header_visible;
    }

    pub fn save_settings(&mut self) -> ExportSettings {
        let export_scale = self.export_scale;
        let header_visible = self.header_visible;
        ExportSettings { export_scale, header_visible }
    }

    pub fn update_export_scale(&mut self, scale: Option<ExportScale>) {
        self.export_scale = scale;
        nuts::send_to::<ExportController, _>(SaveSettingsEvent);
    }

    pub fn update_export_header(&mut self, visible: bool) {
        self.header_visible = visible;
        nuts::send_to::<ExportController, _>(SaveSettingsEvent);
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

        let context = check!(self.canvas.get_context("2d").ok().flatten()
            .and_then(|obj| obj.dyn_into::<web_sys::CanvasRenderingContext2d>().ok()));

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

        let mut header_area = Rect::new(0.0, 0.0, 0.0, 0.0);
        let mut header_baseline = 0.0_f64;
        let mut author_width = 0_i32;
        let preposition_text = " by ";

        let title_text = if self.header_visible && !map.title().is_empty() {
            Some(map.title().to_string())
        } else {
            None
        };
        if let Some(ref title_text) = title_text {
            // measure width of the title text
            context.save();
            context.set_font(&format!("bold {}px Overpass, sans-serif", export_scale.title_height()));
            let metrics = check!(context.measure_text(title_text).ok());
            context.restore();

            header_area.width += metrics.width() as f32;
            header_area.height = header_area.height.max(export_scale.title_height() as f32);
            header_baseline = header_baseline.max(metrics.actual_bounding_box_ascent());
        }

        let author_text = if self.header_visible && !map.author().is_empty() {
            Some(map.author().to_string())
        } else {
            None
        };
        if let Some(ref author_text) = author_text {
            // measure width of the author text
            context.save();
            context.set_font(&format!("bold {}px Overpass, sans-serif", export_scale.author_height()));
            let metrics = check!(context.measure_text(author_text).ok());
            let preposition_width = check!(context.measure_text(preposition_text).ok()).width() as f32;
            context.restore();

            header_area.width += metrics.width() as f32 + preposition_width;
            header_area.height = header_area.height.max(export_scale.author_height() as f32);
            header_baseline = header_baseline.max(metrics.actual_bounding_box_ascent());
            author_width = metrics.width().floor() as i32;
        }

        header_area.width = header_area.width.ceil();
        header_area.height = header_area.height.ceil();

        let width = f32::max(map_area.width, header_area.width) as i32 + 2 * PADDING;
        let mut height = map_area.height as i32 + 2 * PADDING;
        if header_area.height > 0.0 {
            height += header_area.height as i32 + PADDING;
        }
        check!(self.canvas.set_attribute("width", &width.to_string()).ok());
        check!(self.canvas.set_attribute("height", &height.to_string()).ok());

        let mut origin = export_layout.origin() - Point(map_area.left, map_area.top) +
            Point(PADDING as f32, PADDING as f32);
        if header_area.width > map_area.width {
            // center-align map with title text
            origin = origin + Point((header_area.width - map_area.width) / 2.0, 0.0);
        }
        if header_area.height > 0.0 {
            // move map below title text
            origin = origin + Point(0.0, header_area.height + PADDING as f32);
        }
        let export_layout = export_layout.with_origin(origin);

        context.save();
        // draw background color
        context.set_fill_style(&JsValue::from_str(BACKGROUND_COLOR));
        context.fill_rect(0.0, 0.0, f64::from(width), f64::from(height));
        // draw map header line
        if header_area.height > 0.0 {
            context.begin_path();
            context.set_line_cap("round");
            context.set_line_width(2.0);
            let dash_segments = [0.0, 4.0].iter()
                .map(|&val| JsValue::from_f64(val))
                .collect::<js_sys::Array>();
            check!(context.set_line_dash(&JsValue::from(dash_segments)).ok());
            context.set_line_dash_offset(2.0);
            context.move_to(f64::from(PADDING), header_baseline + f64::from(2 * PADDING));
            context.line_to(f64::from(width - PADDING), header_baseline + f64::from(2 * PADDING));
            context.set_stroke_style(&JsValue::from_str(MAP_BORDER_COLOR));
            context.stroke();
        }
        // draw map title text
        if let Some(ref title_text) = title_text {
            context.set_font(&format!("bold {}px Overpass, sans-serif", export_scale.title_height()));
            context.set_text_align("left");
            context.set_line_width(6.0);
            context.set_stroke_style(&JsValue::from_str(BACKGROUND_COLOR));
            check!(context.stroke_text(title_text, f64::from(PADDING), header_baseline + f64::from(PADDING)).ok());
            context.set_fill_style(&JsValue::from_str(MAP_TITLE_COLOR));
            check!(context.fill_text(title_text, f64::from(PADDING), header_baseline + f64::from(PADDING)).ok());
        }
        // draw map author text
        if let Some(ref author_text) = author_text {
            context.set_font(&format!("bold {}px Overpass, sans-serif", export_scale.author_height()));
            context.set_text_align("right");
            context.set_line_width(6.0);
            context.set_stroke_style(&JsValue::from_str(BACKGROUND_COLOR));
            check!(context.stroke_text(author_text, f64::from(width - PADDING), header_baseline + f64::from(PADDING)).ok());
            check!(context.stroke_text(preposition_text, f64::from(width - author_width - PADDING), header_baseline + f64::from(PADDING)).ok());
            context.set_fill_style(&JsValue::from_str(MAP_AUTHOR_COLOR));
            check!(context.fill_text(author_text, f64::from(width - PADDING), header_baseline + f64::from(PADDING)).ok());
            context.set_fill_style(&JsValue::from_str(MAP_PREPOSITION_COLOR));
            check!(context.fill_text(preposition_text, f64::from(width - author_width - PADDING), header_baseline + f64::from(PADDING)).ok());
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
                let mut text = tile.id().base().to_string();
                if tile.has_flat_tokens() {
                    text.push('*');
                }
                check!(draw_tile_label(&context, &text, pos, export_scale.tile_label_height()).ok());
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
