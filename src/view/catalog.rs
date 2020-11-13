//----------------------------------------------------------------------------
//! Rendering of the tile catalog and according event handlers.
//
// $Id$
//----------------------------------------------------------------------------

use log::{info};
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Document, Element};

use crate::check;
use crate::controller::*;
use crate::tile::*;
use super::*;

//----------------------------------------------------------------------------

struct CatalogTile {
    inner: Element,
    id: TileId,
    dragstart_cb: Closure<dyn Fn(web_sys::DragEvent)>,
}

impl CatalogTile {
    fn new(document: &Document, layout: &Layout, id: TileId) -> Result<Self> {
        let canvas = document.create_element_ns(SVG_NS, "svg")?;
        let width = (2.0 * layout.size().x()).round() as i32;
        let height = (2.0 * layout.size().y()).round() as i32;
        canvas.set_attribute("width", &format!("{}px", width))?;
        canvas.set_attribute("height", &format!("{}px", height))?;
        canvas.set_attribute("viewBox", &format!("0 0 {} {}", width, height))?;

        let style = document.create_element_ns(SVG_NS, "style")?;
        style.append_child(&document.create_text_node(TILE_STYLE))?;
        canvas.append_child(&style)?;
        canvas.append_child(&draw_tile(&document, &layout, id, (0, 0), Direction::A)?.into())?;

        let tile = document.create_element("div")?;
        tile.set_attribute("class", "tile")?;
        tile.set_attribute("draggable", "true")?;
        tile.append_child(&canvas)?;

        let drag_img = canvas.clone();
        let dragstart_cb = Closure::wrap(Box::new(move |event: web_sys::DragEvent| {
            if let Some(trans) = event.data_transfer() {
                let data = id.base().to_string();
                trans.set_data("application/rekee", &data).unwrap();
                trans.set_data("text/plain", &data).unwrap();
                trans.set_effect_allowed("copy");
                trans.set_drag_image(&drag_img, 50, 50);
            }
            nuts::publish(DragCatalogBeginEvent { tile: id.base() });
        }) as Box<dyn Fn(_)>);
        tile.add_event_listener_with_callback("dragstart", dragstart_cb.as_ref().unchecked_ref()).unwrap();

        Ok(CatalogTile { inner: tile, id, dragstart_cb })
    }
}

impl AsRef<Element> for CatalogTile {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

impl Drop for CatalogTile {
    fn drop(&mut self) {
        self.inner.remove_event_listener_with_callback("dragstart",
            self.dragstart_cb.as_ref().unchecked_ref()).unwrap();
    }
}

//----------------------------------------------------------------------------

pub struct CatalogView {
    layout: Layout,
    tiles: Vec<CatalogTile>,
    catalog: Element,
    map: Option<Element>,
    dragover_cb: Closure<dyn Fn(web_sys::DragEvent)>,
    dragdrop_cb: Closure<dyn Fn(web_sys::DragEvent)>,
}

impl CatalogView {
    pub fn new(parent: Element, layout: &Layout) -> Result<Self> {
        // create layout instance without map offset
        let layout = Layout::new(layout.orientation(), layout.size(), layout.size());

        let document = parent.owner_document().unwrap();

        // remove all pre-existing child nodes
        let range = document.create_range()?;
        range.select_node_contents(&parent)?;
        range.delete_contents()?;

        let catalog = document.create_element("ul")?;
        catalog.set_id("catalog");
        catalog.set_attribute("class", "mt-2")?;

        let mut tiles = Vec::with_capacity(TileInfo::iter().count());

        for info in TileInfo::iter() {
            let tile = CatalogTile::new(&document, &layout, info.full_id())?;
            let item = document.create_element("li")?;
            item.append_child(tile.as_ref())?;
            catalog.append_child(&item)?;
            tiles.push(tile);
        }
        parent.append_child(&catalog)?;

        let dragover_cb = Closure::wrap(Box::new(move |event: web_sys::DragEvent| {
            let data = event.data_transfer()
                .and_then(|trans| trans.get_data("application/rekee").ok());
            if data.is_some() {
                event.prevent_default();
                let pos = check!(mouse_position(&event));
                nuts::publish(DragMapMoveEvent { pos });
            }
        }) as Box<dyn Fn(_)>);

        let dragdrop_cb = Closure::wrap(Box::new(move |event: web_sys::DragEvent| {
            let tile: Option<TileId> = event.data_transfer()
                .and_then(|trans| trans.get_data("application/rekee").ok())
                .and_then(|data| data.parse().ok());
            if let Some(tile) = tile {
                event.prevent_default();
                let pos = check!(mouse_position(&event));
                nuts::publish(DragCatalogEndEvent { pos, tile });
                nuts::publish(UpdateSelectedEvent { pos });
            }
        }) as Box<dyn Fn(_)>);

        Ok(CatalogView { layout, tiles, catalog, map: None, dragover_cb, dragdrop_cb })
    }

    pub fn drag_begin(&mut self, tile: TileId) {
        info!("drag begin: {:?}", tile);
        if self.map.is_none() {
            // lookup map element and initialize drag-n-drop event handlers
            let document = self.catalog.owner_document().unwrap();
            if let Some(map) = document.get_element_by_id("map") {
                check!(map.add_event_listener_with_callback("dragover",
                    self.dragover_cb.as_ref().unchecked_ref()).ok());
                check!(map.add_event_listener_with_callback("drop",
                    self.dragdrop_cb.as_ref().unchecked_ref()).ok());
                self.map = Some(map);
            }
        }
    }
}

//----------------------------------------------------------------------------
