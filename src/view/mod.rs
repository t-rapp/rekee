//----------------------------------------------------------------------------
//! Rendering of objects and common functions.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

#![allow(clippy::needless_return)]

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Document, Element};

use crate::hexagon::*;
use crate::map::PlacedTile;
use crate::map::PlacedToken;
use crate::tile::*;
use crate::token::TokenId;

mod catalog;
mod catalog_config;
mod export;
mod map;
mod map_config;
mod track_info;
mod version;
mod welcome;

//----------------------------------------------------------------------------

pub type CatalogSettings = catalog::CatalogSettings;
pub type CatalogView = catalog::CatalogView;

pub type CatalogConfigView = catalog_config::CatalogConfigView;

pub type ExportView = export::ExportView;

pub type MapSettings = map::MapSettings;
pub type MapView = map::MapView;

pub type MapConfigView = map_config::MapConfigView;

pub type TrackInfoView = track_info::TrackInfoView;

pub type VersionView = version::VersionView;

pub type WelcomeView = welcome::WelcomeView;

type Result<T> = std::result::Result<T, JsValue>;

//----------------------------------------------------------------------------

const SVG_NS: Option<&str> = Some(SVG_NS_STR);
const SVG_NS_STR: &str = "http://www.w3.org/2000/svg";

const TILE_STYLE: &str = include_str!("tile.css");

//----------------------------------------------------------------------------

fn tile_image_size(layout: &Layout) -> Point {
    let rect = layout.with_orientation(Orientation::flat())
        .hexagon_rect(Coordinate::new(0, 0));
    Point(rect.width, rect.height)
}

fn tile_image_center(layout: &Layout) -> Point {
    let size = tile_image_size(layout);
    Point(0.5 * size.x(), 0.5 * size.y())
}

fn token_image_size(layout: &Layout, token: &PlacedToken) -> Point {
    match token.id {
        TokenId::Chicane(_) | TokenId::ChicaneWithLimit(_) | TokenId::Jump(_) | TokenId::Water(_) =>
            Point(0.41 * layout.size().x(), 0.28 * layout.size().y()),
        TokenId::Finish | TokenId::JokerEntrance | TokenId::JokerExit =>
            Point(0.82 * layout.size().x(), 0.40 * layout.size().y()),
    }
}

fn token_image_center(layout: &Layout, token: &PlacedToken) -> Point {
    let size = token_image_size(layout, token);
    match token.id {
        TokenId::Chicane(_) | TokenId::ChicaneWithLimit(_) | TokenId::Jump(_) | TokenId::Water(_) =>
            Point(0.5 * size.x(), 0.5 * size.y()),
        TokenId::Finish | TokenId::JokerEntrance | TokenId::JokerExit =>
            Point(0.5 * size.x(), size.y()),
    }
}

//----------------------------------------------------------------------------

fn draw_tile(document: &Document, layout: &Layout, tile: &PlacedTile) -> Result<Element> {
    let pos = tile.pos.to_pixel(layout);
    let parent = document.create_element_ns(SVG_NS, "g")?;
    parent.set_attribute("class", "tile")?;
    parent.set_attribute("transform", &format!("translate({:.3} {:.3})", pos.x(), pos.y()))?;

    let size = tile_image_size(layout);
    let center = tile_image_center(layout);
    let angle = layout.direction_to_angle(tile.dir);
    let img = document.create_element_ns(SVG_NS, "image")?;
    img.set_attribute("href", &format!("tiles/thumb-{}.png", tile.id()))?;
    img.set_attribute("width", &format!("{}", size.x()))?;
    img.set_attribute("height", &format!("{}", size.y()))?;
    img.set_attribute("image-rendering", "optimizeQuality")?;
    img.set_attribute("transform", &format!("rotate({:.0}) translate({:.3} {:.3})", angle, -center.x(), -center.y()))?;
    parent.append_child(&img)?;

    Ok(parent)
}

fn draw_tile_with_label(document: &Document, layout: &Layout, tile: &PlacedTile) -> Result<Element> {
    let parent = draw_tile(document, layout, tile)?;

    let label = document.create_element_ns(SVG_NS, "text")?;
    label.set_attribute("class", "label")?;
    label.set_attribute("x", "0")?;
    label.set_attribute("y", "0")?;
    let mut text = tile.id().base().to_string();
    if !tile.tokens.is_empty() {
        text.push('*');
    }
    label.append_child(&document.create_text_node(&text))?;
    parent.append_child(&label)?;

    Ok(parent)
}

fn draw_tile_label(document: &Document, layout: &Layout, tile: &PlacedTile) -> Result<Element> {
    let pos = tile.pos.to_pixel(layout);
    let parent = document.create_element_ns(SVG_NS, "g")?;
    parent.set_attribute("class", "tile")?;
    parent.set_attribute("transform", &format!("translate({:.3} {:.3})", pos.x(), pos.y()))?;

    let label = document.create_element_ns(SVG_NS, "text")?;
    label.set_attribute("class", "label")?;
    label.set_attribute("x", "0")?;
    label.set_attribute("y", "0")?;
    let mut text = tile.id().base().to_string();
    if !tile.tokens.is_empty() {
        text.push('*');
    }
    label.append_child(&document.create_text_node(&text))?;
    parent.append_child(&label)?;

    Ok(parent)
}

fn draw_tile_token(document: &Document, layout: &Layout, tile: &PlacedTile, token: &PlacedToken) -> Result<Element> {
    let pos = (FloatCoordinate::from(tile.pos) + token.pos.rotate(tile.dir)).to_pixel(layout);
    let parent = document.create_element_ns(SVG_NS, "g")?;
    parent.set_attribute("class", "token")?;
    parent.set_attribute("transform", &format!("translate({:.3} {:.3})", pos.x(), pos.y()))?;

    let size = token_image_size(layout, token);
    let center = token_image_center(layout, token);
    let angle = layout.direction_to_angle(FloatDirection::from(tile.dir) + token.dir);
    let img = document.create_element_ns(SVG_NS, "image")?;
    img.set_attribute("href", &format!("tokens/{:x}.png", token.id))?;
    img.set_attribute("width", &format!("{}", size.x()))?;
    img.set_attribute("height", &format!("{}", size.y()))?;
    img.set_attribute("image-rendering", "optimizeQuality")?;
    img.set_attribute("transform", &format!("rotate({:.1}) translate({:.3} {:.3})", angle, -center.x(), -center.y()))?;
    parent.append_child(&img)?;

    Ok(parent)
}

//----------------------------------------------------------------------------

fn mouse_position(event: &web_sys::MouseEvent) -> Option<Point> {
    let element = event.current_target()
        .and_then(|target| target.dyn_into::<web_sys::Element>().ok())?;
    let rect = element.get_bounding_client_rect();
    let x = event.client_x() as f32 - rect.left() as f32;
    let y = event.client_y() as f32 - rect.top() as f32;
    Some(Point(x, y))
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
