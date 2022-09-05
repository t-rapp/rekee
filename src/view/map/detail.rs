//----------------------------------------------------------------------------
//! Display of map details and configuration of track tokens.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Document, Element};

use crate::check;
use crate::controller::*;
use super::*;

//----------------------------------------------------------------------------

fn draw_grid_hex<C>(document: &Document, layout: &Layout, pos: C) -> Result<Element>
    where C: Into<Coordinate>
{
    let corners = layout.hexagon_corners(pos.into());
    let points: Vec<String> = corners.iter()
        .map(|p| format!("{:.1},{:.1}", p.x(), p.y()))
        .collect();
    let hex = document.create_element_ns(SVG_NS, "polygon")?;
    hex.set_attribute("class", "hex")?;
    hex.set_attribute("points", &points.join(" "))?;
    Ok(hex)
}

//----------------------------------------------------------------------------

pub struct MapDetailView {
    inner: Element,
    layout: Layout,
    grid: Element,
    tiles: Element,
    tokens: Element,
    apply: Element,
    close: Element,
    close_cb: Closure<dyn Fn(web_sys::Event)>,
    keydown_cb: Closure<dyn Fn(web_sys::KeyboardEvent)>,
}

impl MapDetailView {
    pub fn new(parent: Element, layout: &Layout) -> Result<Self> {
        let document = parent.owner_document().unwrap();
        let inner = parent;
        let detail_size = layout.size() * 3.0;
        let detail_origin = detail_size * 1.5;
        let layout = Layout::new(layout.orientation(), detail_size, detail_origin);

        let columns = inner.query_selector(".columns")?
            .ok_or("Cannot find columns of map detail element")?;

        let column = document.create_element("div")?;
        column.set_attribute("class", "column")?;
        columns.append_child(&column)?;

        let canvas_size = layout.origin() * 2.0;
        let canvas_viewbox = Rect::new(0.0, 0.0, canvas_size.x(), canvas_size.y());

        let canvas = document.create_element_ns(SVG_NS, "svg")?;
        canvas.set_id("map-detail");
        canvas.set_attribute("width", &format!("{:.0}px", canvas_viewbox.width))?;
        canvas.set_attribute("height", &format!("{:.0}px", canvas_viewbox.height))?;
        canvas.set_attribute("viewBox", &format!("{:.0} {:.0} {:.0} {:.0}",
            canvas_viewbox.left, canvas_viewbox.top,
            canvas_viewbox.width, canvas_viewbox.height))?;
        canvas.set_attribute("xmlns", SVG_NS_STR)?;
        column.append_child(&canvas)?;

        let style = document.create_element_ns(SVG_NS, "style")?;
        style.append_child(&document.create_text_node(MAP_STYLE))?;
        style.append_child(&document.create_text_node(TILE_STYLE))?;
        canvas.append_child(&style)?;

        let tiles = document.create_element_ns(SVG_NS, "g")?;
        tiles.set_attribute("class", "tiles")?;
        canvas.append_child(&tiles)?;

        // Drawing of the grid is after tiles here, different to the normal map:
        // Allows to use the grid lines as a visual hint for token placement.
        let grid = document.create_element_ns(SVG_NS, "g")?;
        grid.set_attribute("class", "grid")?;
        let center = Coordinate::new(0, 0);
        grid.append_child(&draw_grid_hex(&document, &layout, center)?.into())?;
        for &dir in Direction::iter() {
            let pos = center.neighbor(dir);
            grid.append_child(&draw_grid_hex(&document, &layout, pos)?.into())?;
        }
        canvas.append_child(&grid)?;

        let tokens = document.create_element_ns(SVG_NS, "g")?;
        tokens.set_attribute("class", "tokens")?;
        canvas.append_child(&tokens)?;

        let apply = document.get_element_by_id("apply-map-detail")
            .ok_or("Cannot find apply button of map detail element")?;
        let close = inner.query_selector(".modal-close")?
            .ok_or("Cannot find close button of map detail element")?;

        let close_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::publish(HideMapDetailEvent);
        }) as Box<dyn Fn(_)>);
        apply.add_event_listener_with_callback("click", close_cb.as_ref().unchecked_ref())?;
        close.add_event_listener_with_callback("click", close_cb.as_ref().unchecked_ref())?;

        let keydown_cb = Closure::wrap(Box::new(move |event: web_sys::KeyboardEvent| {
            if event.repeat() {
                return;
            }
            if event.key().eq_ignore_ascii_case("escape") {
                nuts::publish(HideMapDetailEvent);
            }
        }) as Box<dyn Fn(_)>);
        document.add_event_listener_with_callback("keydown", keydown_cb.as_ref().unchecked_ref())?;

        Ok(MapDetailView { inner, layout, grid, tiles, tokens, apply, close, close_cb, keydown_cb })
    }

    pub fn set_active(&self, value: bool) {
        if value {
            check!(self.inner.class_list().add_1("is-active").ok());
        } else {
            check!(self.inner.class_list().remove_1("is-active").ok());
        }
    }

    pub fn update_background_grid(&mut self, visible: bool) {
        if visible != !self.grid.hidden() {
            self.grid.set_hidden(!visible);
        }
    }

    pub fn update_map_tiles(&mut self, map: &Map, center: Coordinate) {
        let document = self.tiles.owner_document().unwrap();

        // remove all existing tiles and tokens
        let range = check!(document.create_range().ok());
        check!(range.select_node_contents(&self.tiles).ok());
        check!(range.delete_contents().ok());
        check!(range.select_node_contents(&self.tokens).ok());
        check!(range.delete_contents().ok());

        let offset = self.layout.origin() - center.to_pixel(&self.layout);
        let tile_layout = self.layout.with_origin(self.layout.origin() + offset);

        // draw selected tile
        if let Some(tile) = map.get(center) {
            if let Ok(el) = draw_tile(&document, &tile_layout, tile) {
                self.tiles.append_child(&el).unwrap();
            }
            for token in &tile.tokens {
                if let Ok(el) = draw_tile_token(&document, &tile_layout, tile, token) {
                    self.tokens.append_child(&el).unwrap();
                }
            }
        }

        // draw tiles around the selected tile
        for &dir in Direction::iter() {
            let pos = center.neighbor(dir);
            if let Some(tile) = map.get(pos) {
                if let Ok(el) = draw_tile(&document, &tile_layout, tile) {
                    self.tiles.append_child(&el).unwrap();
                }
                for token in &tile.tokens {
                    if let Ok(el) = draw_tile_token(&document, &tile_layout, tile, token) {
                        self.tokens.append_child(&el).unwrap();
                    }
                }
            }
        }
    }

}

impl Drop for MapDetailView {
    fn drop(&mut self) {
        let document = self.inner.owner_document().unwrap();

        let _ = self.apply.remove_event_listener_with_callback("click",
            self.close_cb.as_ref().unchecked_ref());
        let _ = self.close.remove_event_listener_with_callback("click",
            self.close_cb.as_ref().unchecked_ref());
        let _ = document.remove_event_listener_with_callback("keydown",
            self.keydown_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------
