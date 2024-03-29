//----------------------------------------------------------------------------
//! Rendering of objects and common functions.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

#![allow(clippy::needless_return)]

use std::fmt;
use std::ops::Deref;
use std::str::FromStr;

use serde::{Serialize, Deserialize};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Document, Element};

use crate::export::util;
use crate::hexagon::{Coordinate, FloatCoordinate, FloatDirection, Layout, Point};
use crate::map::PlacedTile;
use crate::map::PlacedToken;
use crate::tile::{DangerLevel, Terrain};
use crate::token::TokenId;

mod catalog;
mod catalog_config;
mod export;
mod map;
mod track_info;
mod version;
mod welcome;

//----------------------------------------------------------------------------

pub type CatalogSettings = catalog::CatalogSettings;
pub type CatalogView = catalog::CatalogView;

pub type CatalogConfigView = catalog_config::CatalogConfigView;

pub type ExportSettings = export::ExportSettings;
pub type ExportView = export::ExportView;

pub type MapSettings = map::MapSettings;
pub type MapView = map::MapView;

pub type MapConfigView = map::config::MapConfigView;

pub type MapDetailView = map::detail::MapDetailView;

pub type TrackInfoView = track_info::TrackInfoView;

pub type VersionView = version::VersionView;

pub type WelcomeView = welcome::WelcomeView;

type Result<T> = std::result::Result<T, JsValue>;

//----------------------------------------------------------------------------

const SVG_NS: Option<&str> = Some(SVG_NS_STR);
const SVG_NS_STR: &str = "http://www.w3.org/2000/svg";

const TILE_STYLE: &str = include_str!("tile.css");

const PACENOTE_LINE_HEIGHT: i32 = 18;
const PACENOTE_WRAP_WIDTH: usize = 12;

//----------------------------------------------------------------------------

fn mouse_position(event: &web_sys::MouseEvent) -> Option<Point> {
    let element = event.current_target()
        .and_then(|target| target.dyn_into::<web_sys::Element>().ok())?;
    mouse_position_element(event, &element)
}

fn mouse_position_element(event: &web_sys::MouseEvent, element: &Element) -> Option<Point> {
    let rect = element.get_bounding_client_rect();
    let x = event.client_x() as f32 - rect.left() as f32;
    let y = event.client_y() as f32 - rect.top() as f32;
    Some(Point(x, y))
}

fn tile_number_text(tile: &PlacedTile) -> String {
    let mut text = tile.id().base().to_string();
    if tile.has_flat_tokens() {
        text.push('*');
    }
    text
}

fn tile_pacenote_text(tile: &PlacedTile, wrap_width: usize) -> Vec<String> {
    let mut text: Vec<String> = Vec::with_capacity(4);
    let mut line = String::new();
    for pacenote in tile.pacenotes() {
        let pacenote = pacenote.to_string();
        if !line.is_empty() && line.len() + pacenote.len() + 1 >= wrap_width {
            line.push(',');
            text.push(line);
            line = String::new();
        }
        if !line.is_empty() {
            line.push(',');
            line.push(' ');
        }
        line.push_str(&pacenote);
    }
    if !line.is_empty() {
        text.push(line);
    }
    text
}

// Helper macro that checks an option result and aborts the current function in case of an error.
#[doc(hidden)]
#[macro_export]
macro_rules! check {
    ($e:expr) => {
        match $e {
            None => return,
            Some(val) => val,
        }
    };
}

//----------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "lowercase", try_from = "&str")]
pub enum LabelType {
    None,
    Number,
    Pacenote,
}

impl Default for LabelType {
    fn default() -> Self {
        LabelType::Number
    }
}

impl fmt::Display for LabelType {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LabelType::None =>
                write!(fmt, "None")?,
            LabelType::Number =>
                write!(fmt, "Number")?,
            LabelType::Pacenote =>
                write!(fmt, "Pacenote")?,
        }
        Ok(())
    }
}

impl FromStr for LabelType {
    type Err = ParseLabelTypeError;

    fn from_str(val: &str) -> std::result::Result<Self, Self::Err> {
        // match strings from both trait implementations, std::fmt::Display and serde::Serialize
        let s = val.to_ascii_lowercase();

        match s.as_ref() {
            "none" =>
                Ok(LabelType::None),
            "number" =>
                Ok(LabelType::Number),
            "pacenote" =>
                Ok(LabelType::Pacenote),
            _ =>
                Err(ParseLabelTypeError::Unknown(val.to_string())),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseLabelTypeError {
    Unknown(String),
}

impl fmt::Display for ParseLabelTypeError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseLabelTypeError::Unknown(val) =>
                write!(fmt, "Unknown label type value \"{}\"", val),
        }
    }
}

impl std::convert::TryFrom<&str> for LabelType {
    type Error = ParseLabelTypeError;

    fn try_from(value: &str) -> std::result::Result<Self, Self::Error> {
        LabelType::from_str(value)
    }
}

//----------------------------------------------------------------------------

struct TileImageElement {
    inner: Element,
}

impl TileImageElement {
    pub fn new(document: &Document, layout: &Layout, tile: &PlacedTile) -> Result<Self> {
        let pos = tile.pos.to_pixel(layout);
        let inner = document.create_element_ns(SVG_NS, "g")?;
        inner.set_attribute("class", "tile")?;
        if let Some(danger_level) = tile.danger_level() {
            inner.set_danger_level_class(danger_level);
        }
        inner.set_attribute("transform", &format!("translate({:.1} {:.1})", pos.x(), pos.y()))?;

        let size = util::tile_image_size(layout);
        let center = util::tile_image_center(layout);
        let angle = layout.direction_to_angle(tile.dir);
        let image = document.create_element_ns(SVG_NS, "image")?;
        if layout.size().x() >= 150.0 || layout.size().y() >= 150.0 {
            image.set_attribute("href", &format!("tiles/tile-{:x}.webp", tile.id()))?;
        } else {
            image.set_attribute("href", &format!("tiles/thumbs/tile-{:x}.webp", tile.id()))?;
        }
        image.set_attribute("width", &format!("{:.0}", size.x()))?;
        image.set_attribute("height", &format!("{:.0}", size.y()))?;
        image.set_attribute("image-rendering", "optimizeQuality")?;
        image.set_attribute("transform", &format!("rotate({:.0}) translate({:.1} {:.1})", angle, -center.x(), -center.y()))?;
        inner.append_child(&image)?;

        Ok(TileImageElement { inner })
    }

    pub fn new_with_label(document: &Document, layout: &Layout, tile: &PlacedTile) -> Result<Self> {
        let element = Self::new(document, layout, tile)?;

        let label = TileLabelElement::create_label_element(document, tile)?;
        element.append_child(&label)?;

        let pacenote = TilePacenoteElement::create_pacenote_element(document, tile)?;
        element.append_child(&pacenote)?;

        Ok(element)
    }
}

impl Deref for TileImageElement {
    type Target = Element;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

//----------------------------------------------------------------------------

struct TileLabelElement {
    inner: Element,
}

impl TileLabelElement {
    pub fn new(document: &Document, layout: &Layout, tile: &PlacedTile) -> Result<Self> {
        let pos = tile.pos.to_pixel(layout);
        let inner = document.create_element_ns(SVG_NS, "g")?;
        inner.set_attribute("class", "tile")?;
        if let Some(danger_level) = tile.danger_level() {
            inner.set_danger_level_class(danger_level);
        }
        inner.set_attribute("transform", &format!("translate({:.1} {:.1})", pos.x(), pos.y()))?;

        let label = Self::create_label_element(document, tile)?;
        inner.append_child(&label)?;

        Ok(TileLabelElement { inner })
    }

    fn create_label_element(document: &Document, tile: &PlacedTile) -> Result<Element> {
        let label = document.create_element_ns(SVG_NS, "text")?;
        label.set_attribute("class", "label")?;
        label.set_attribute("x", "0")?;
        label.set_attribute("y", "0")?;
        let text = tile_number_text(tile);
        label.append_child(&document.create_text_node(&text))?;

        Ok(label)
    }
}

impl Deref for TileLabelElement {
    type Target = Element;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

//----------------------------------------------------------------------------

struct TilePacenoteElement {
    inner: Element,
}

impl TilePacenoteElement {
    pub fn new(document: &Document, layout: &Layout, tile: &PlacedTile) -> Result<Self> {
        let pos = tile.pos.to_pixel(layout);
        let inner = document.create_element_ns(SVG_NS, "g")?;
        inner.set_attribute("class", "tile")?;
        if let Some(danger_level) = tile.danger_level() {
            inner.set_danger_level_class(danger_level);
        }
        inner.set_attribute("transform", &format!("translate({:.1} {:.1})", pos.x(), pos.y()))?;

        let pacenote = Self::create_pacenote_element(document, tile)?;
        inner.append_child(&pacenote)?;

        Ok(TilePacenoteElement { inner })
    }

    fn create_pacenote_element(document: &Document, tile: &PlacedTile) -> Result<Element> {
        let label = document.create_element_ns(SVG_NS, "text")?;
        label.set_attribute("class", "pacenote")?;
        label.set_attribute("x", "0")?;
        label.set_attribute("y", "0")?;

        let text = tile_pacenote_text(tile, PACENOTE_WRAP_WIDTH);
        let text_height = PACENOTE_LINE_HEIGHT * text.len().saturating_sub(1) as i32;
        let mut line_y = -text_height / 2;
        for line in &text {
            let tspan = document.create_element_ns(SVG_NS, "tspan")?;
            tspan.append_child(&document.create_text_node(line))?;
            tspan.set_attribute("x", "0")?;
            tspan.set_attribute("y", &line_y.to_string())?;
            label.append_child(&tspan)?;
            line_y += PACENOTE_LINE_HEIGHT;
        }

        Ok(label)
    }
}

impl Deref for TilePacenoteElement {
    type Target = Element;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

//----------------------------------------------------------------------------

struct TokenImageElement {
    inner: Element,
    image: Element,
}

impl TokenImageElement {
    pub fn new(document: &Document, layout: &Layout, tile: &PlacedTile, token: &PlacedToken) -> Result<Self> {
        let inner = document.create_element_ns(SVG_NS, "g")?;
        inner.set_attribute("class", "token")?;

        let image = document.create_element_ns(SVG_NS, "image")?;
        image.set_attribute("image-rendering", "optimizeQuality")?;
        inner.append_child(&image)?;

        let element = TokenImageElement { inner, image };
        element.set_token(layout, tile, token);
        Ok(element)
    }

    pub fn set_token(&self, layout: &Layout, tile: &PlacedTile, token: &PlacedToken) {
        let pos = (FloatCoordinate::from(tile.pos) + token.pos.rotate(tile.dir)).to_pixel(layout);
        check!(self.inner.set_attribute("transform", &format!("translate({:.3} {:.3})", pos.x(), pos.y())).ok());

        let size = util::token_image_size(layout, token.id);
        let center = util::token_image_center(layout, token.id);
        let angle = layout.direction_to_angle(FloatDirection::from(tile.dir) + token.dir);
        if layout.size().x() >= 150.0 || layout.size().y() >= 150.0 {
            check!(self.image.set_attribute("href", &format!("tokens/{:x}.webp", token.id)).ok());
        } else {
            check!(self.image.set_attribute("href", &format!("tokens/thumbs/{:x}.webp", token.id)).ok());
        }
        check!(self.image.set_attribute("width", &format!("{}", size.x())).ok());
        check!(self.image.set_attribute("height", &format!("{}", size.y())).ok());
        check!(self.image.set_attribute("transform", &format!("rotate({:.1}) translate({:.3} {:.3})", angle, -center.x(), -center.y())).ok());
    }

    pub fn set_token_pos(&self, layout: &Layout, tile: &PlacedTile, token_pos: FloatCoordinate) {
        let pos = (FloatCoordinate::from(tile.pos) + token_pos.rotate(tile.dir)).to_pixel(layout);
        check!(self.inner.set_attribute("transform", &format!("translate({:.3} {:.3})", pos.x(), pos.y())).ok());
    }
}

impl Deref for TokenImageElement {
    type Target = Element;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

//----------------------------------------------------------------------------

trait ElementHidden {
    fn hidden(&self) -> bool;
    fn set_hidden(&self, value: bool);
}

impl<T: AsRef<Element>> ElementHidden for T {
    fn hidden(&self) -> bool {
        let elm = self.as_ref();
        elm.class_list().contains("is-hidden")
    }

    fn set_hidden(&self, value: bool) {
        let elm = self.as_ref();
        if value {
            check!(elm.class_list().add_1("is-hidden").ok());
        } else {
            check!(elm.class_list().remove_1("is-hidden").ok());
        }
    }
}

//----------------------------------------------------------------------------

trait ElementTextInputTarget {
    fn is_text_input_target(&self) -> bool;
}

impl<T: AsRef<Element>> ElementTextInputTarget for T {
    fn is_text_input_target(&self) -> bool {
        let element = self.as_ref();
        // element is an editable input control
        element.dyn_ref::<web_sys::HtmlInputElement>()
            .map_or(false, |element| {
                let non_editable_types = ["button", "file", "hidden", "reset", "submit"];
                !element.read_only() && !non_editable_types.contains(&element.type_().as_ref())
            }) ||
        // element is an text-area (memo) input control
        element.dyn_ref::<web_sys::HtmlTextAreaElement>()
            .map_or(false, |element| !element.read_only()) ||
        // element is a drop-down selection list
        element.has_type::<web_sys::HtmlSelectElement>() ||
        // custom element which was made editable
        element.dyn_ref::<web_sys::HtmlElement>()
            .map_or(false, |element| element.is_content_editable())
    }
}

//----------------------------------------------------------------------------

trait ElementDangerLevel {
    fn set_danger_level_class(&self, danger_level: DangerLevel);
}

impl<T: AsRef<Element>> ElementDangerLevel for T {
    fn set_danger_level_class(&self, danger_level: DangerLevel) {
        let element = self.as_ref();
        let class_name = format!("danger-level-{:x}", danger_level);
        check!(element.class_list().add_1(&class_name).ok());
    }
}

//----------------------------------------------------------------------------
