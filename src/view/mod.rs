//----------------------------------------------------------------------------
//! Rendering of objects and common functions.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

#![allow(clippy::needless_return)]

use std::ops::Deref;

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
mod track_info;
mod version;
mod welcome;

//----------------------------------------------------------------------------

pub type CatalogSettings = catalog::CatalogSettings;
pub type CatalogView = catalog::CatalogView;

pub type CatalogConfigView = catalog_config::CatalogConfigView;

pub type ExportScale = export::ExportScale;
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

//----------------------------------------------------------------------------

fn tile_image_size(layout: &Layout) -> Point {
    // use flat layout orientation as we confine us to the image size without rotation applied yet
    let rect = layout.with_orientation(Orientation::flat())
        .hexagon_rect(Coordinate::new(0, 0));
    Point(rect.width.round(), rect.height.round())
}

fn tile_image_center(layout: &Layout) -> Point {
    let size = tile_image_size(layout);
    Point(0.5 * size.x(), 0.5 * size.y())
}

fn token_image_size(layout: &Layout, token_id: TokenId) -> Point {
    match token_id {
        TokenId::Chicane(_) | TokenId::ChicaneWithLimit(_) | TokenId::Jump(_) | TokenId::Water(_) =>
            Point(0.500 * layout.size().x(), 0.350 * layout.size().y()),
        TokenId::ClimbAscent | TokenId::ClimbDescent | TokenId::Cloud | TokenId::Oxygen(_) =>
            Point(0.315 * layout.size().x(), 0.315 * layout.size().y()),
        TokenId::Finish | TokenId::JokerEntrance | TokenId::JokerExit =>
            Point(1.200 * layout.size().x(), 0.585 * layout.size().y()),
    }
}

fn token_image_center(layout: &Layout, token_id: TokenId) -> Point {
    let size = token_image_size(layout, token_id);
    match token_id {
        TokenId::Chicane(_) | TokenId::ChicaneWithLimit(_) | TokenId::Jump(_) | TokenId::Water(_) =>
            Point(0.5 * size.x(), 0.5 * size.y()),
        TokenId::ClimbAscent | TokenId::ClimbDescent | TokenId::Cloud | TokenId::Oxygen(_) =>
            Point(0.5 * size.x(), 0.5 * size.y()),
        TokenId::Finish | TokenId::JokerEntrance | TokenId::JokerExit =>
            Point(0.5 * size.x(), size.y()),
    }
}

//----------------------------------------------------------------------------


fn draw_tile_label(document: &Document, layout: &Layout, tile: &PlacedTile) -> Result<Element> {
    let pos = tile.pos.to_pixel(layout);
    let parent = document.create_element_ns(SVG_NS, "g")?;
    parent.set_attribute("class", "tile")?;
    parent.set_attribute("transform", &format!("translate({:.1} {:.1})", pos.x(), pos.y()))?;

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

struct TileImageElement {
    inner: Element,
}

impl TileImageElement {
    pub fn new(document: &Document, layout: &Layout, tile: &PlacedTile) -> Result<Self> {
        let pos = tile.pos.to_pixel(layout);
        let inner = document.create_element_ns(SVG_NS, "g")?;
        inner.set_attribute("class", "tile")?;
        inner.set_attribute("transform", &format!("translate({:.1} {:.1})", pos.x(), pos.y()))?;

        let size = tile_image_size(layout);
        let center = tile_image_center(layout);
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

        let label = document.create_element_ns(SVG_NS, "text")?;
        label.set_attribute("class", "label")?;
        label.set_attribute("x", "0")?;
        label.set_attribute("y", "0")?;
        let mut text = tile.id().base().to_string();
        if !tile.tokens.is_empty() {
            text.push('*');
        }
        label.append_child(&document.create_text_node(&text))?;
        element.append_child(&label)?;

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

        let size = token_image_size(layout, token.id);
        let center = token_image_center(layout, token.id);
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

#[cfg(test)]
mod tests {
    use approx::assert_abs_diff_eq;
    use super::*;

    #[test]
    fn tile_image_size_and_center() {
        let layout = Layout::new(Orientation::pointy(), Point(60.0, 60.0), Point(0.0, 0.0));
        let size = tile_image_size(&layout);
        assert_abs_diff_eq!(size, Point(120.0, 104.0));
        let center = tile_image_center(&layout);
        assert_abs_diff_eq!(center, Point(60.0, 52.0));

        let layout = Layout::new(Orientation::flat(), Point(60.0, 60.0), Point(0.0, 0.0));
        let size = tile_image_size(&layout);
        assert_abs_diff_eq!(size, Point(120.0, 104.0));
        let center = tile_image_center(&layout);
        assert_abs_diff_eq!(center, Point(60.0, 52.0));
    }

    #[test]
    fn token_image_size_and_center() {
        let layout = Layout::new(Orientation::pointy(), Point(60.0, 60.0), Point(0.0, 0.0));

        let token_id = TokenId::Chicane(Terrain::Asphalt);
        let size = token_image_size(&layout, token_id);
        assert_abs_diff_eq!(size, Point(30.0, 21.0));
        let center = token_image_center(&layout, token_id);
        assert_abs_diff_eq!(center, Point(15.0, 10.5));

        let token_id = TokenId::ClimbAscent;
        let size = token_image_size(&layout, token_id);
        assert_abs_diff_eq!(size, Point(18.9, 18.9));
        let center = token_image_center(&layout, token_id);
        assert_abs_diff_eq!(center, Point(9.45, 9.45));

        let token_id = TokenId::JokerEntrance;
        let size = token_image_size(&layout, token_id);
        assert_abs_diff_eq!(size, Point(72.0, 35.1));
        let center = token_image_center(&layout, token_id);
        assert_abs_diff_eq!(center, Point(36.0, 35.1));

        let layout = Layout::new(Orientation::flat(), Point(60.0, 60.0), Point(0.0, 0.0));

        let token_id = TokenId::Chicane(Terrain::Asphalt);
        let size = token_image_size(&layout, token_id);
        assert_abs_diff_eq!(size, Point(30.0, 21.0));
        let center = token_image_center(&layout, token_id);
        assert_abs_diff_eq!(center, Point(15.0, 10.5));

        let token_id = TokenId::ClimbAscent;
        let size = token_image_size(&layout, token_id);
        assert_abs_diff_eq!(size, Point(18.9, 18.9));
        let center = token_image_center(&layout, token_id);
        assert_abs_diff_eq!(center, Point(9.45, 9.45));

        let token_id = TokenId::JokerExit;
        let size = token_image_size(&layout, token_id);
        assert_abs_diff_eq!(size, Point(72.0, 35.1));
        let center = token_image_center(&layout, token_id);
        assert_abs_diff_eq!(center, Point(36.0, 35.1));
    }
}

//----------------------------------------------------------------------------
