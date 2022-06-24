//----------------------------------------------------------------------------
//! Definition and distribution of user events.
//!
//! Uses the "nuts" library for sending events between global instances of view
//! components. Acts as a replacement for the JavaScript event loop.
//!
//! See <https://www.jakobmeier.ch/blogging/Rust_on_the_Web.html> for a nice
//! summary of the Rust and JavaScript programming patterns. The epilogue of that
//! article directly inspired this module.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use nuts::{DefaultDomain, DomainState};

use crate::edition::Edition;
use crate::hexagon::{Coordinate, Direction, Point};
use crate::map::{Map, PlacedTile};
use crate::storage::Storage;
use crate::tile::{ConnectionHint, Terrain, TileId};
use crate::view::*;

//----------------------------------------------------------------------------

pub struct LoadSettingsEvent;

pub struct SaveSettingsEvent;

pub struct ImportFileEvent {
    pub map: Map,
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

pub struct RotateMapLeftEvent;

pub struct RotateMapRightEvent;

pub struct UpdateTitleEvent {
    pub title: String,
}

pub struct UpdateBackgroundGridEvent {
    pub visible: bool,
}

pub struct UpdateTileLabelsEvent {
    pub visible: bool,
}

pub struct UpdateSelectedEvent {
    pub pos: Point,
}

pub struct RotateSelectedLeftEvent;

pub struct RotateSelectedRightEvent;

pub struct RemoveSelectedEvent;

pub struct UpdateLanesFilterEvent {
    pub lanes: Option<u8>,
}

pub struct UpdateTerrainFilterEvent {
    pub terrain: Option<Terrain>,
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

pub struct UpdateCatalogEditionsEvent {
    pub editions: Vec<Edition>,
}

pub struct CatalogController {
    view: CatalogView,
    storage: Storage,
}

impl CatalogController {
    pub fn init(view: CatalogView) {
        let storage = Storage::new("catalog");
        let controller = CatalogController { view, storage };
        let activity = nuts::new_domained_activity(controller, &DefaultDomain);

        // register private events
        activity.private_channel(|controller, event| {
            controller.load_settings(&event);
        });
        activity.private_channel(|controller, event| {
            controller.save_settings(&event);
        });

        // register public events
        activity.subscribe(CatalogController::load_settings);
        activity.subscribe(CatalogController::save_settings);
        activity.subscribe(CatalogController::import_file);
        activity.subscribe(CatalogController::update_catalog_editions);
        activity.subscribe(CatalogController::update_lanes_filter);
        activity.subscribe(CatalogController::update_terrain_filter);
        activity.subscribe(CatalogController::update_tile_labels);
        activity.subscribe(CatalogController::update_tile_usage);
        activity.subscribe(CatalogController::drag_catalog_begin);
    }

    fn load_settings(&mut self, _event: &LoadSettingsEvent) {
        let settings = self.storage.get();
        if let Some(settings) = settings {
            self.view.load_settings(&settings);
        }
    }

    fn save_settings(&mut self, _event: &SaveSettingsEvent) {
        let settings = self.view.save_settings();
        self.storage.set(&settings);
        nuts::store_to_domain(&DefaultDomain, settings);
    }

    fn import_file(&mut self, event: &ImportFileEvent) {
        self.view.import_file(&event.map);
    }

    fn update_catalog_editions(&mut self, event: &UpdateCatalogEditionsEvent) {
        self.view.update_editions(&event.editions);
    }

    fn update_lanes_filter(&mut self, event: &UpdateLanesFilterEvent) {
        self.view.update_lanes_filter(event.lanes);
    }

    fn update_terrain_filter(&mut self, event: &UpdateTerrainFilterEvent) {
        self.view.update_terrain_filter(event.terrain);
    }

    fn update_tile_labels(&mut self, event: &UpdateTileLabelsEvent) {
        self.view.update_tile_labels(event.visible);
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
    storage: Storage,
}

impl MapController {
    pub fn init(view: MapView) {
        let storage = Storage::new("map");
        let controller = MapController { view, storage };
        let activity = nuts::new_domained_activity(controller, &DefaultDomain);

        // register private events
        activity.private_channel(|controller, event| {
            controller.load_settings(&event);
        });
        activity.private_channel(|controller, event| {
            controller.save_settings(&event);
        });

        // register public events
        activity.subscribe(MapController::load_settings);
        activity.subscribe(MapController::save_settings);
        activity.subscribe(MapController::import_file);
        activity.subscribe(MapController::export_file);
        activity.subscribe(MapController::insert_tile);
        activity.subscribe(MapController::append_tile);
        activity.subscribe(MapController::align_center);
        activity.subscribe(MapController::clear_map);
        activity.subscribe(MapController::rotate_map_left);
        activity.subscribe(MapController::rotate_map_right);
        activity.subscribe(MapController::update_title);
        activity.subscribe(MapController::update_background_grid);
        activity.subscribe(MapController::update_tile_labels);
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

    fn load_settings(&mut self, _event: &LoadSettingsEvent) {
        let settings = self.storage.get();
        if let Some(settings) = settings {
            self.view.load_settings(&settings);
        }
    }

    fn save_settings(&mut self, _event: &SaveSettingsEvent) {
        let settings = self.view.save_settings();
        self.storage.set(&settings);
        nuts::store_to_domain(&DefaultDomain, settings);
    }

    fn import_file(&mut self, event: &ImportFileEvent) {
        self.view.import_file(&event.map);
    }

    fn export_file(&mut self, _event: &ExportFileEvent) {
        self.view.export_file();
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

    fn rotate_map_left(&mut self, _event: &RotateMapLeftEvent) {
        self.view.rotate_map_left();
    }

    fn rotate_map_right(&mut self, _event: &RotateMapRightEvent) {
        self.view.rotate_map_right();
    }

    fn update_title(&mut self, event: &UpdateTitleEvent) {
        self.view.update_title(&event.title);
    }

    fn update_background_grid(&mut self, event: &UpdateBackgroundGridEvent) {
        self.view.update_background_grid(event.visible);
    }

    fn update_tile_labels(&mut self, event: &UpdateTileLabelsEvent) {
        self.view.update_tile_labels(event.visible);
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

pub struct ShowCatalogConfigEvent;

pub struct HideCatalogConfigEvent;

pub struct ToggleCatalogEditionEvent {
    pub edition: Edition,
}

pub struct ApplyCatalogEditionsEvent;

pub struct CatalogConfigController {
    view: CatalogConfigView,
}

impl CatalogConfigController {
    pub fn init(view: CatalogConfigView) {
        let controller = CatalogConfigController { view };
        let activity = nuts::new_domained_activity(controller, &DefaultDomain);

        // register private events
        activity.private_channel(|controller, event: ToggleCatalogEditionEvent| {
            controller.view.toggle_edition(event.edition);
        });
        activity.private_channel(|controller, _event: ApplyCatalogEditionsEvent| {
            controller.view.apply_catalog_editions();
        });

        // register public events
        activity.subscribe_domained(CatalogConfigController::show);
        activity.subscribe(CatalogConfigController::hide);
    }

    fn show(&mut self, domain: &mut DomainState, _event: &ShowCatalogConfigEvent) {
        if let Some(settings) = domain.try_get::<CatalogSettings>() {
            self.view.set_editions(&settings.editions);
        }
        self.view.set_active(true);
    }

    fn hide(&mut self, _event: &HideCatalogConfigEvent) {
        self.view.set_active(false);
    }
}

//----------------------------------------------------------------------------

pub struct ShowTrackInfoEvent;

pub struct HideTrackInfoEvent;

pub struct TrackInfoController {
    view: TrackInfoView,
}

impl TrackInfoController {
    pub fn init(view: TrackInfoView) {
        let controller = TrackInfoController { view };
        let activity = nuts::new_domained_activity(controller, &DefaultDomain);

        // register public events
        activity.subscribe_domained(Self::show);
        activity.subscribe(Self::hide);
    }

    fn show(&mut self, domain: &mut DomainState, _event: &ShowTrackInfoEvent) {
        if let Some(settings) = domain.try_get::<MapSettings>() {
            self.view.update_map_tiles(settings.map.tiles());
        }
        self.view.set_active(true);
    }

    fn hide(&mut self, _event: &HideTrackInfoEvent) {
        self.view.set_active(false);
    }
}

//----------------------------------------------------------------------------

pub struct ShowMapConfigEvent;

pub struct HideMapConfigEvent;

pub struct ApplyMapConfigEvent;

pub struct MapConfigController {
    view: MapConfigView,
}

impl MapConfigController {
    pub fn init(view: MapConfigView) {
        let controller = MapConfigController { view };
        let activity = nuts::new_domained_activity(controller, &DefaultDomain);

        // register private events
        activity.private_channel(|controller, _event: ApplyMapConfigEvent| {
            controller.view.apply_map_config();
        });

        // register public events
        activity.subscribe_domained(Self::show);
        activity.subscribe(Self::hide);
    }

    fn show(&mut self, domain: &mut DomainState, _event: &ShowMapConfigEvent) {
        if let Some(settings) = domain.try_get::<MapSettings>() {
            self.view.set_background_grid(settings.background_grid_visible);
            self.view.set_tile_labels(settings.tile_labels_visible);
        }
        self.view.set_active(true);
    }

    fn hide(&mut self, _event: &HideMapConfigEvent) {
        self.view.set_active(false);
    }
}

//----------------------------------------------------------------------------

pub struct DrawExportTileDoneEvent {
    pub tile: PlacedTile,
}

pub struct ExportController {
    view: ExportView,
}

impl ExportController {
    pub fn init(view: ExportView) {
        let controller = ExportController { view };
        let activity = nuts::new_domained_activity(controller, &DefaultDomain);

        // register private events
        activity.private_channel(|controller, event: DrawExportTileDoneEvent| {
            controller.view.draw_export_tile_done(&event.tile);
        });

        // register public events
        activity.subscribe_domained(ExportController::export_image);
    }

    fn export_image(&mut self, domain: &mut DomainState, _event: &ExportImageEvent) {
        if let Some(settings) = domain.try_get::<MapSettings>() {
            self.view.draw_export_image(&settings.map, settings.tile_labels_visible);
        }
    }
}

//----------------------------------------------------------------------------

pub struct WelcomeController {
    view: WelcomeView,
    storage: Storage,
}

impl WelcomeController {
    pub fn init(view: WelcomeView) {
        let storage = Storage::with_session_storage("welcome");
        let controller = WelcomeController { view, storage };
        let activity = nuts::new_domained_activity(controller, &DefaultDomain);

        // register private events
        activity.private_channel(|controller, event| {
            controller.load_settings(&event);
        });
        activity.private_channel(|controller, event| {
            controller.save_settings(&event);
        });

        // register public events
        activity.subscribe(WelcomeController::load_settings);
        activity.subscribe(WelcomeController::save_settings);
        activity.subscribe(WelcomeController::import_file);
        activity.subscribe(WelcomeController::insert_tile);
        activity.subscribe(WelcomeController::append_tile);
        activity.subscribe(WelcomeController::drag_catalog_end);
        activity.subscribe(WelcomeController::drag_map_end);
        activity.subscribe(WelcomeController::show_welcome);
        activity.subscribe(WelcomeController::hide_welcome);
    }

    fn load_settings(&mut self, _event: &LoadSettingsEvent) {
        let settings = self.storage.get();
        if let Some(settings) = settings {
            self.view.load_settings(&settings);
        }
    }

    fn save_settings(&mut self, _event: &SaveSettingsEvent) {
        let settings = self.view.save_settings();
        self.storage.set(&settings);
        nuts::store_to_domain(&DefaultDomain, settings);
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
