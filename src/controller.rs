//----------------------------------------------------------------------------
//! Definition and distribution of user events.
//
// $Id$
//----------------------------------------------------------------------------

use crate::hexagon::*;
use crate::tile::*;
use crate::view::{CatalogView, PageView};

pub struct ImportFileEvent {
    pub data: String,
}

pub struct InsertTileEvent {
    pub id: TileId,
    pub pos: Coordinate,
    pub dir: Direction,
}

pub struct AppendTileEvent {
    pub id: TileId,
    pub hint: Option<ConnectionHint>,
}

pub struct AlignCenterEvent;

pub struct ClearMapEvent;

pub struct UpdateMapEvent;

pub struct UpdateSelectedEvent {
    pub pos: Point,
}

pub struct RotateSelectedLeftEvent;

pub struct RotateSelectedRightEvent;

pub struct RemoveSelectedEvent;

pub struct DragCatalogBeginEvent {
    pub tile: TileId,
}

pub struct DragCatalogEndEvent {
    pub pos: Point,
    pub tile: TileId,
}

pub struct DragMapBeginEvent {
    pub pos: Point,
}

pub struct DragMapMoveEvent {
    pub pos: Point,
}

pub struct DragMapEndEvent {
    pub pos: Point,
}

pub struct DragMapCancelEvent;

//----------------------------------------------------------------------------

pub struct CatalogController {
    view: CatalogView,
}

impl CatalogController {
    pub fn init(view: CatalogView) {
        let controller = CatalogController { view };
        let catalog = nuts::new_activity(controller);
        catalog.subscribe(CatalogController::drag_catalog_begin);
    }

    fn drag_catalog_begin(&mut self, event: &DragCatalogBeginEvent) {
        self.view.drag_begin(event.tile);
    }
}

//----------------------------------------------------------------------------

pub struct PageController {
    view: PageView,
}

impl PageController {
    pub fn init(view: PageView) {
        let controller = PageController { view };
        let page = nuts::new_activity(controller);
        page.subscribe(PageController::import_file);
        page.subscribe(PageController::insert_tile);
        page.subscribe(PageController::append_tile);
        page.subscribe(PageController::align_center);
        page.subscribe(PageController::clear_map);
        page.subscribe(PageController::update_map);
        page.subscribe(PageController::update_selected);
        page.subscribe(PageController::rotate_selected_left);
        page.subscribe(PageController::rotate_selected_right);
        page.subscribe(PageController::remove_selected);
        page.subscribe(PageController::drag_catalog_end);
        page.subscribe(PageController::drag_map_begin);
        page.subscribe(PageController::drag_map_move);
        page.subscribe(PageController::drag_map_end);
        page.subscribe(PageController::drag_map_cancel);
    }

    fn import_file(&mut self, event: &ImportFileEvent) {
        self.view.import_file(&event.data);
    }

    fn insert_tile(&mut self, event: &InsertTileEvent) {
        self.view.insert_tile(event.id, event.pos, event.dir);
    }

    fn append_tile(&mut self, event: &AppendTileEvent) {
        self.view.append_tile(event.id, event.hint);
    }

    fn align_center(&mut self, _event: &AlignCenterEvent) {
        self.view.align_center();
    }

    fn clear_map(&mut self, _event: &ClearMapEvent) {
        self.view.clear_map();
    }

    fn update_map(&mut self, _event: &UpdateMapEvent) {
        self.view.update_map();
    }

    fn update_selected(&mut self, event: &UpdateSelectedEvent) {
        self.view.update_selected(event.pos);
    }

    fn rotate_selected_left(&mut self, _event: &RotateSelectedLeftEvent) {
        self.view.rotate_selected_left();
    }

    fn rotate_selected_right(&mut self, _event: &RotateSelectedRightEvent) {
        self.view.rotate_selected_right();
    }

    fn remove_selected(&mut self, _event: &RemoveSelectedEvent) {
        self.view.remove_selected();
    }

    fn drag_catalog_end(&mut self, event: &DragCatalogEndEvent) {
        self.view.drag_end(event.pos, Some(event.tile));
    }

    fn drag_map_begin(&mut self, event: &DragMapBeginEvent) {
        self.view.drag_begin(event.pos);
    }

    fn drag_map_move(&mut self, event: &DragMapMoveEvent) {
        self.view.drag_move(event.pos);
    }

    fn drag_map_end(&mut self, event: &DragMapEndEvent) {
        self.view.drag_end(event.pos, None);
    }

    fn drag_map_cancel(&mut self, _event: &DragMapCancelEvent) {
        self.view.drag_cancel();
    }
}

//----------------------------------------------------------------------------
