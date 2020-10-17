//----------------------------------------------------------------------------
//! Definition and distribution of user events.
//
// $Id$
//----------------------------------------------------------------------------

use crate::hexagon::*;
use crate::tile::*;
use crate::view::PageView;

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

pub struct UpdateMapEvent;

pub struct UpdateSelectedEvent {
    pub pos: Point,
}

//----------------------------------------------------------------------------

struct PageController {
    view: PageView,
}

impl PageController {
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

    fn update_map(&mut self, _event: &UpdateMapEvent) {
        self.view.update_map();
    }

    fn update_selected(&mut self, event: &UpdateSelectedEvent) {
        self.view.update_selected(event.pos);
    }
}

//----------------------------------------------------------------------------

pub fn init(view: PageView) {
    let controller = PageController { view };
    let page = nuts::new_activity(controller);
    page.subscribe(PageController::import_file);
    page.subscribe(PageController::insert_tile);
    page.subscribe(PageController::append_tile);
    page.subscribe(PageController::align_center);
    page.subscribe(PageController::update_map);
    page.subscribe(PageController::update_selected);
}

//----------------------------------------------------------------------------
