//----------------------------------------------------------------------------
//! Definition and distribution of user events.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use crate::hexagon::*;
use crate::tile::*;
use crate::view::*;

//----------------------------------------------------------------------------

pub struct ImportFileEvent {
    pub data: String,
}

pub struct ExportFileEvent;

pub struct ExportImageEvent;

pub struct InsertTileEvent {
    pub id: TileId,
    pub pos: Coordinate,
    pub dir: Direction,
}

pub struct AppendTileEvent {
    pub id: TileId,
    pub pos: Option<Coordinate>,
    pub hint: Option<ConnectionHint>,
}

pub struct AlignCenterEvent;

pub struct ClearMapEvent;

pub struct UpdateMapEvent;

pub struct UpdateTitleEvent {
    pub title: String,
}

pub struct UpdateSelectedEvent {
    pub pos: Point,
}

pub struct RotateSelectedLeftEvent;

pub struct RotateSelectedRightEvent;

pub struct RemoveSelectedEvent;

pub struct UpdateFilterEvent {
    pub lanes: Option<u8>,
}

pub struct UpdateTileUsageEvent {
    pub tiles: Vec<TileId>,
}

pub struct UpdateConnectionHintEvent {
    pub hint: Option<ConnectionHint>,
}

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

pub struct ShowWelcomeEvent;

pub struct HideWelcomeEvent;

//----------------------------------------------------------------------------

pub struct CatalogController {
    view: CatalogView,
}

impl CatalogController {
    pub fn init(view: CatalogView) {
        let controller = CatalogController { view };
        let activity = nuts::new_activity(controller);
        activity.subscribe(CatalogController::update_filter);
        activity.subscribe(CatalogController::update_tile_usage);
        activity.subscribe(CatalogController::drag_catalog_begin);
    }

    fn update_filter(&mut self, event: &UpdateFilterEvent) {
        self.view.update_filter(event.lanes);
    }

    fn update_tile_usage(&mut self, event: &UpdateTileUsageEvent) {
        self.view.update_tile_usage(&event.tiles);
    }

    fn drag_catalog_begin(&mut self, event: &DragCatalogBeginEvent) {
        self.view.drag_begin(event.tile);
    }
}

//----------------------------------------------------------------------------

pub struct MapController {
    view: MapView,
}

impl MapController {
    pub fn init(view: MapView) {
        let controller = MapController { view };
        let activity = nuts::new_activity(controller);
        activity.subscribe(MapController::import_file);
        activity.subscribe(MapController::export_file);
        activity.subscribe(MapController::export_image);
        activity.subscribe(MapController::insert_tile);
        activity.subscribe(MapController::append_tile);
        activity.subscribe(MapController::align_center);
        activity.subscribe(MapController::clear_map);
        activity.subscribe(MapController::update_map);
        activity.subscribe(MapController::update_title);
        activity.subscribe(MapController::update_selected);
        activity.subscribe(MapController::rotate_selected_left);
        activity.subscribe(MapController::rotate_selected_right);
        activity.subscribe(MapController::remove_selected);
        activity.subscribe(MapController::drag_catalog_end);
        activity.subscribe(MapController::drag_map_begin);
        activity.subscribe(MapController::drag_map_move);
        activity.subscribe(MapController::drag_map_end);
        activity.subscribe(MapController::drag_map_cancel);
        activity.subscribe(MapController::update_connection_hint);
    }

    fn import_file(&mut self, event: &ImportFileEvent) {
        self.view.import_file(&event.data);
    }

    fn export_file(&mut self, _event: &ExportFileEvent) {
        self.view.export_file();
    }

    fn export_image(&mut self, _event: &ExportImageEvent) {
        self.view.export_image();
    }

    fn insert_tile(&mut self, event: &InsertTileEvent) {
        self.view.insert_tile(event.id, event.pos, event.dir);
    }

    fn append_tile(&mut self, event: &AppendTileEvent) {
        self.view.append_tile(event.id, event.pos, event.hint);
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

    fn update_title(&mut self, event: &UpdateTitleEvent) {
        self.view.update_title(&event.title);
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

    fn update_connection_hint(&mut self, event: &UpdateConnectionHintEvent) {
        self.view.update_connection_hint(event.hint);
    }
}

//----------------------------------------------------------------------------

pub struct DrawExportImageEvent {
    pub map: Map,
}

pub struct DrawExportTileDoneEvent {
    pub tile: PlacedTile,
}

pub struct ExportController {
    view: ExportView,
}

impl ExportController {
    pub fn init(view: ExportView) {
        let controller = ExportController { view };
        let activity = nuts::new_activity(controller);
        activity.subscribe(ExportController::draw_export_image);
        activity.subscribe(ExportController::draw_export_tile_done);
    }

    fn draw_export_image(&mut self, event: &DrawExportImageEvent) {
        self.view.draw_export_image(&event.map);
    }

    fn draw_export_tile_done(&mut self, event: &DrawExportTileDoneEvent) {
        self.view.draw_export_tile_done(&event.tile);
    }
}

//----------------------------------------------------------------------------

pub struct WelcomeController {
    view: WelcomeView,
}

impl WelcomeController {
    pub fn init(view: WelcomeView) {
        let controller = WelcomeController { view };
        let activity = nuts::new_activity(controller);
        activity.subscribe(WelcomeController::import_file);
        activity.subscribe(WelcomeController::insert_tile);
        activity.subscribe(WelcomeController::append_tile);
        activity.subscribe(WelcomeController::drag_catalog_end);
        activity.subscribe(WelcomeController::drag_map_end);
        activity.subscribe(WelcomeController::show_welcome);
        activity.subscribe(WelcomeController::hide_welcome);
    }

    fn import_file(&mut self, _event: &ImportFileEvent) {
        self.view.set_hidden(true);
    }

    fn insert_tile(&mut self, _event: &InsertTileEvent) {
        self.view.set_hidden(true);
    }

    fn append_tile(&mut self, _event: &AppendTileEvent) {
        self.view.set_hidden(true);
    }

    fn drag_catalog_end(&mut self, _event: &DragCatalogEndEvent) {
        self.view.set_hidden(true);
    }

    fn drag_map_end(&mut self, _event: &DragMapEndEvent) {
        self.view.set_hidden(true);
    }

    fn show_welcome(&mut self, _event: &ShowWelcomeEvent) {
        self.view.set_hidden(false);
    }

    fn hide_welcome(&mut self, _event: &HideWelcomeEvent) {
        self.view.set_hidden(true);
    }
}

//----------------------------------------------------------------------------
