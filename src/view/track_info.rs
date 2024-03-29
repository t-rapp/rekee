//----------------------------------------------------------------------------
//! Information about tiles of the current track.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Element};

use crate::check;
use crate::controller::track_info::*;
use crate::edition::Edition;
use crate::map::{PlacedTile, PlacedTileList};
use crate::tile::TileList;
use super::*;

//----------------------------------------------------------------------------

pub struct TrackInfoView {
    inner: Element,
    close: Element,
    close_cb: Closure<dyn Fn(web_sys::Event)>,
    keydown_cb: Closure<dyn Fn(web_sys::KeyboardEvent)>,
}

impl TrackInfoView {
    pub fn new(parent: Element) -> Result<Self> {
        let inner = parent;

        let close_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::publish(HideTrackInfoEvent);
        }) as Box<dyn Fn(_)>);

        let close = inner.query_selector(".button")?
            .ok_or("Cannot find close button of track info element")?;
        close.add_event_listener_with_callback("click", close_cb.as_ref().unchecked_ref())?;

        let close = inner.query_selector(".modal-close")?
            .ok_or("Cannot find close button of catalog config element")?;
        close.add_event_listener_with_callback("click", close_cb.as_ref().unchecked_ref())?;

        let keydown_cb = Closure::wrap(Box::new(move |event: web_sys::KeyboardEvent| {
            if event.repeat() {
                return;
            }
            if event.key().eq_ignore_ascii_case("enter") && (event.ctrl_key() || event.meta_key()) {
                nuts::publish(HideTrackInfoEvent);
            }
            if event.key().eq_ignore_ascii_case("escape") {
                nuts::publish(HideTrackInfoEvent);
            }
        }) as Box<dyn Fn(_)>);

        Ok(TrackInfoView { inner, close, close_cb, keydown_cb })
    }

    pub fn set_active(&self, value: bool) {
        let document = self.inner.owner_document().unwrap();

        if value {
            check!(self.inner.class_list().add_1("is-active").ok());
            check!(document.add_event_listener_with_callback("keydown", self.keydown_cb.as_ref().unchecked_ref()).ok());
        } else {
            check!(self.inner.class_list().remove_1("is-active").ok());
            check!(document.remove_event_listener_with_callback("keydown", self.keydown_cb.as_ref().unchecked_ref()).ok());
        }
    }

    pub fn update_map_tiles(&self, map_tiles: &[PlacedTile]) {
        let document = self.inner.owner_document().unwrap();

        if let Some(parent) = self.inner.query_selector("#track-info-edition-data").ok().flatten() {
            // remove all previous data
            let range = check!(document.create_range().ok());
            check!(range.select_node_contents(&parent).ok());
            check!(range.delete_contents().ok());

            let list = check!(document.create_element("ul").ok());
            check!(parent.append_child(&list).ok());

            let summary = PlacedTileList::edition_summary(map_tiles);
            for row in summary {
                let mut info = String::new();
                if row.edition_count > 1 {
                    info.push_str(&row.edition_count.to_string());
                    info.push('×');
                }
                if row.tile_count > 0 {
                    if !info.is_empty() {
                        info.push_str(", ");
                    }
                    info.push_str(&row.tile_count.to_string());
                    if row.tile_count == 1 {
                        info.push_str(" tile");
                    } else {
                        info.push_str(" tiles");
                    }
                }
                if row.token_count > 0 {
                    if !info.is_empty() {
                        info.push_str(", ");
                    }
                    info.push_str(&row.token_count.to_string());
                    if row.edition == Some(Edition::DirtRx) {
                        if row.token_count == 1 {
                            info.push_str(" standee");
                        } else {
                            info.push_str(" standees");
                        }
                    } else if row.token_count == 1 {
                        info.push_str(" token");
                    } else {
                        info.push_str(" tokens");
                    }
                }

                let mut text = match row.edition {
                    Some(val) => val.to_string(),
                    None => "Unknown".to_string(),
                };
                text.push_str(" (");
                text.push_str(&info);
                text.push(')');

                let item = check!(document.create_element("li").ok());
                check!(item.append_child(&document.create_text_node(&text)).ok());
                check!(list.append_child(&item).ok());
            }
        }

        let mut track_tiles = Vec::with_capacity(map_tiles.len());
        let mut filler_tiles = Vec::with_capacity(map_tiles.len());
        for tile in map_tiles.iter() {
            if tile.terrain() == Some(Terrain::None) {
                filler_tiles.push(tile.id());
            } else {
                track_tiles.push(tile.id());
            }
        }

        if let Some(parent) = self.inner.query_selector("#track-info-length-data").ok().flatten() {
            // remove all previous data
            let range = check!(document.create_range().ok());
            check!(range.select_node_contents(&parent).ok());
            check!(range.delete_contents().ok());

            let lines = check!(document.create_element("p").ok());
            check!(parent.append_child(&lines).ok());

            let mut text = String::new();
            if !track_tiles.is_empty() {
                text.push_str(&track_tiles.len().to_string());
                text.push_str(" track ");
                if track_tiles.len() == 1 {
                    text.push_str("tile");
                } else {
                    text.push_str("tiles");
                }
            }
            if !filler_tiles.is_empty() {
                if !text.is_empty() {
                    text.push_str(", ");
                }
                text.push_str(&filler_tiles.len().to_string());
                text.push_str(" filler ");
                if filler_tiles.len() == 1 {
                    text.push_str("tile");
                } else {
                    text.push_str("tiles");
                }
            }
            check!(lines.append_child(&document.create_text_node(&text)).ok());
            let linebreak = check!(document.create_element("br").ok());
            check!(lines.append_child(&linebreak).ok());
        }

        if let Some(parent) = self.inner.query_selector("#track-info-terrain-data").ok().flatten() {
            // remove all previous data
            let range = check!(document.create_range().ok());
            check!(range.select_node_contents(&parent).ok());
            check!(range.delete_contents().ok());

            let list = check!(document.create_element("ul").ok());
            check!(parent.append_child(&list).ok());

            let total_count = track_tiles.len() as u32;
            let summary = track_tiles.terrain_summary();
            for row in summary {
                let mut text = match row.terrain {
                    Some(val) => val.to_string(),
                    None => "Unknown".to_string(),
                };
                text.push_str(" (");
                text.push_str(&row.tile_count.to_string());
                if row.tile_count == 1 {
                    text.push_str(" tile");
                } else {
                    text.push_str(" tiles");
                }
                assert!(total_count > 0);
                let percent = row.tile_count * 100 / total_count;
                text.push_str(", ");
                text.push_str(&percent.to_string());
                text.push_str("%)");

                let item = check!(document.create_element("li").ok());
                check!(item.append_child(&document.create_text_node(&text)).ok());
                check!(list.append_child(&item).ok());
            }
        }

        if let Some(parent) = self.inner.query_selector("#track-info-danger-level-data").ok().flatten() {
            // remove all previous data
            let range = check!(document.create_range().ok());
            check!(range.select_node_contents(&parent).ok());
            check!(range.delete_contents().ok());

            let list = check!(document.create_element("ul").ok());
            check!(parent.append_child(&list).ok());

            let total_count = track_tiles.len() as u32;
            let summary = track_tiles.danger_level_summary();
            for row in summary {
                let mut text = match row.danger_level {
                    Some(val) => val.to_string(),
                    None => "Unknown".to_string(),
                };
                text.push_str(" (");
                text.push_str(&row.tile_count.to_string());
                if row.tile_count == 1 {
                    text.push_str(" tile");
                } else {
                    text.push_str(" tiles");
                }
                assert!(total_count > 0);
                let percent = row.tile_count * 100 / total_count;
                text.push_str(", ");
                text.push_str(&percent.to_string());
                text.push_str("%)");

                let item = check!(document.create_element("li").ok());
                check!(item.append_child(&document.create_text_node(&text)).ok());
                check!(list.append_child(&item).ok());
            }
        }
    }
}

impl Drop for TrackInfoView {
    fn drop(&mut self) {
        let document = self.inner.owner_document().unwrap();

        let _ = self.close.remove_event_listener_with_callback("click",
            self.close_cb.as_ref().unchecked_ref());
        let _ = document.remove_event_listener_with_callback("keydown",
            self.keydown_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------
