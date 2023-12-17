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

use crate::edition::Edition;
use crate::export::{ExportScale, ExportColorScheme};
use crate::hexagon::{Coordinate, Direction, Point};
use crate::map::{Map, PlacedTile, PlacedToken};
use crate::storage::Storage;
use crate::tile::{ConnectionHint, Terrain, TileId};
use crate::token::TokenId;
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

pub struct UpdateAuthorEvent {
    pub author: String,
}

pub struct UpdateBackgroundGridEvent {
    pub visible: bool,
}

pub struct UpdateExportScaleEvent {
    pub scale: Option<ExportScale>,
}

pub struct UpdateExportHeaderEvent {
    pub visible: bool,
}

pub struct UpdateExportListingEvent {
    pub visible: bool,
}

pub struct UpdateExportColorSchemeEvent {
    pub color_scheme: Option<ExportColorScheme>,
}

pub struct UpdateTileLabelsEvent {
    pub label_type: LabelType,
}

pub struct ToggleTileLabelsEvent {
    pub label_type: LabelType,
    pub active: bool,
}

pub struct ToggleTileTokensEvent {
    pub active: bool,
}

pub struct UpdateSelectedTileEvent {
    pub pos: Point,
}

pub struct AddSelectedTileTokenEvent {
    pub token: PlacedToken,
}

pub struct UpdateSelectedTileTokensEvent {
    pub tokens: Vec<PlacedToken>,
}

pub struct RotateSelectedTileLeftEvent;

pub struct RotateSelectedTileRightEvent;

pub struct RemoveSelectedTileEvent;

pub struct UpdateCatalogEditionsEvent {
    pub editions: Vec<Edition>,
}

pub struct UpdateTileUsageEvent {
    pub tiles: Vec<TileId>,
}

pub struct UpdateConnectionHintEvent {
    pub hint: Option<ConnectionHint>,
}

pub struct DragCatalogTileBeginEvent {
    pub tile: TileId,
}

pub struct DragCatalogTileMoveEvent {
    pub pos: Point,
}

pub struct DragCatalogTileEndEvent {
    pub pos: Point,
    pub tile: TileId,
}

//----------------------------------------------------------------------------

pub mod catalog {
    use nuts::DefaultDomain;
    use super::*;

    pub(crate) struct UpdateLanesFilterEvent {
        pub lanes: Option<u8>,
    }

    pub(crate) struct UpdateTerrainFilterEvent {
        pub terrain: Option<Terrain>,
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
            activity.private_channel(|controller, event: UpdateLanesFilterEvent| {
                controller.view.update_lanes_filter(event.lanes);
            });
            activity.private_channel(|controller, event: UpdateTerrainFilterEvent| {
                controller.view.update_terrain_filter(event.terrain);
            });

            // register public events
            activity.subscribe(CatalogController::load_settings);
            activity.subscribe(CatalogController::save_settings);
            activity.subscribe(CatalogController::import_file);
            activity.subscribe(CatalogController::update_catalog_editions);
            activity.subscribe(CatalogController::update_tile_labels);
            activity.subscribe(CatalogController::toggle_tile_labels);
            activity.subscribe(CatalogController::update_tile_usage);
            activity.subscribe(CatalogController::drag_catalog_tile_begin);
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

        fn update_tile_labels(&mut self, event: &UpdateTileLabelsEvent) {
            self.view.update_tile_labels(event.label_type);
        }

        fn toggle_tile_labels(&mut self, event: &ToggleTileLabelsEvent) {
            self.view.toggle_tile_labels(event.label_type, event.active);
        }

        fn update_tile_usage(&mut self, event: &UpdateTileUsageEvent) {
            self.view.update_tile_usage(&event.tiles);
        }

        fn drag_catalog_tile_begin(&mut self, event: &DragCatalogTileBeginEvent) {
            self.view.drag_tile_begin(event.tile);
        }
    }
}

//----------------------------------------------------------------------------

pub mod map {
    use nuts::DefaultDomain;
    use super::*;

    pub(crate) struct DragMapTileBeginEvent {
        pub pos: Point,
    }

    pub(crate) struct DragMapTileMoveEvent {
        pub pos: Point,
    }

    pub(crate) struct DragMapTileEndEvent {
        pub pos: Point,
    }

    pub(crate) struct DragMapTileCancelEvent;

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
            activity.private_channel(|controller, event: DragMapTileBeginEvent| {
                controller.view.drag_tile_begin(event.pos);
            });
            activity.private_channel(|controller, event: DragMapTileMoveEvent| {
                controller.view.drag_tile_move(event.pos);
            });
            activity.private_channel(|controller, event: DragMapTileEndEvent| {
                controller.view.drag_tile_end(event.pos, None);
            });
            activity.private_channel(|controller, _event: DragMapTileCancelEvent| {
                controller.view.drag_tile_cancel();
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
            activity.subscribe(MapController::update_author);
            activity.subscribe(MapController::update_background_grid);
            activity.subscribe(MapController::update_tile_labels);
            activity.subscribe(MapController::toggle_tile_labels);
            activity.subscribe(MapController::toggle_tile_tokens);
            activity.subscribe(MapController::update_selected_tile);
            activity.subscribe(MapController::add_selected_tile_token);
            activity.subscribe(MapController::update_selected_tile_tokens);
            activity.subscribe(MapController::rotate_selected_tile_left);
            activity.subscribe(MapController::rotate_selected_tile_right);
            activity.subscribe(MapController::remove_selected_tile);
            activity.subscribe(MapController::drag_catalog_tile_move);
            activity.subscribe(MapController::drag_catalog_tile_end);
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

        fn update_author(&mut self, event: &UpdateAuthorEvent) {
            self.view.update_author(&event.author);
        }

        fn update_background_grid(&mut self, event: &UpdateBackgroundGridEvent) {
            self.view.update_background_grid(event.visible);
        }

        fn update_tile_labels(&mut self, event: &UpdateTileLabelsEvent) {
            self.view.update_tile_labels(event.label_type);
        }

        fn toggle_tile_labels(&mut self, event: &ToggleTileLabelsEvent) {
            self.view.toggle_tile_labels(event.label_type, event.active);
        }

        fn toggle_tile_tokens(&mut self, event: &ToggleTileTokensEvent) {
            self.view.toggle_tile_tokens(event.active);
        }

        fn update_selected_tile(&mut self, event: &UpdateSelectedTileEvent) {
            self.view.update_selected_tile(event.pos);
        }

        fn add_selected_tile_token(&mut self, event: &AddSelectedTileTokenEvent) {
            self.view.add_selected_tile_token(event.token.clone());
        }

        fn update_selected_tile_tokens(&mut self, event: &UpdateSelectedTileTokensEvent) {
            self.view.update_selected_tile_tokens(&event.tokens);
        }

        fn rotate_selected_tile_left(&mut self, _event: &RotateSelectedTileLeftEvent) {
            self.view.rotate_selected_tile_left();
        }

        fn rotate_selected_tile_right(&mut self, _event: &RotateSelectedTileRightEvent) {
            self.view.rotate_selected_tile_right();
        }

        fn remove_selected_tile(&mut self, _event: &RemoveSelectedTileEvent) {
            self.view.remove_selected_tile();
        }

        fn drag_catalog_tile_move(&mut self, event: &DragCatalogTileMoveEvent) {
            self.view.drag_tile_move(event.pos);
        }

        fn drag_catalog_tile_end(&mut self, event: &DragCatalogTileEndEvent) {
            self.view.drag_tile_end(event.pos, Some(event.tile));
        }

        fn update_connection_hint(&mut self, event: &UpdateConnectionHintEvent) {
            self.view.update_connection_hint(event.hint);
        }
    }
}

//----------------------------------------------------------------------------

pub mod catalog_config {
    use nuts::{DefaultDomain, DomainState};
    use super::*;

    pub struct ShowCatalogConfigEvent;

    pub struct HideCatalogConfigEvent;

    pub(crate) struct ToggleCatalogEditionEvent {
        pub edition: Edition,
    }

    pub(crate) struct ApplyCatalogEditionsEvent;

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
}

//----------------------------------------------------------------------------

pub mod track_info {
    use nuts::{DefaultDomain, DomainState};
    use super::*;

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
}

//----------------------------------------------------------------------------

pub mod map_config {
    use nuts::{DefaultDomain, DomainState};
    use super::*;

    pub struct ShowMapConfigEvent;

    pub struct HideMapConfigEvent;

    pub(crate) struct ApplyMapConfigEvent;

    pub struct MapConfigController {
        view: MapConfigView,
    }

    impl MapConfigController {
        pub fn init(view: MapConfigView) {
            let controller = MapConfigController { view };
            let activity = nuts::new_domained_activity(controller, &DefaultDomain);

            // register private events
            activity.private_channel(|controller, event: UpdateExportScaleEvent| {
                controller.view.set_export_scale(event.scale);
            });
            activity.private_channel(|controller, event: UpdateExportColorSchemeEvent| {
                controller.view.set_export_color_scheme(event.color_scheme);
            });
            activity.private_channel(|controller, event: UpdateTileLabelsEvent| {
                controller.view.set_label_type(event.label_type);
            });
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
                self.view.set_label_type(settings.label_type);
            }
            if let Some(settings) = domain.try_get::<ExportSettings>() {
                self.view.set_export_scale(settings.export_scale);
                self.view.set_export_header(settings.header_visible);
                self.view.set_export_listing(settings.listing_visible);
            }
            self.view.set_active(true);
        }

        fn hide(&mut self, _event: &HideMapConfigEvent) {
            self.view.set_active(false);
        }
    }
}

//----------------------------------------------------------------------------

pub mod map_detail {
    use nuts::{DefaultDomain, DomainState};
    use super::*;

    pub struct ShowMapDetailEvent;

    pub struct HideMapDetailEvent;

    pub(crate) struct AddTokenPropertiesEvent {
        pub token_id: TokenId,
    }

    pub(crate) struct DeleteTokenPropertiesEvent {
        pub index: u32,
    }

    pub(crate) struct ToggleTokenPropertiesEvent {
        pub index: u32,
    }

    pub(crate) struct UpdateTokenTypeEvent {
        pub index: u32,
        pub token_id: TokenId,
    }

    pub(crate) struct UpdateOxygenVariantEvent {
        pub index: u32,
        pub number: u8,
    }

    pub(crate) struct UpdateTokenDistanceEvent {
        pub index: u32,
        pub value: f32,
    }

    pub(crate) struct UpdateTokenAngleEvent {
        pub index: u32,
        pub value: f32,
    }

    pub(crate) struct UpdateTokenOrientationEvent {
        pub index: u32,
        pub value: f32,
    }

    pub(crate) struct DragMapDetailTokenBeginEvent {
        pub index: u32,
        pub pos: Point,
    }

    pub(crate) struct DragMapDetailTokenMoveEvent {
        pub pos: Point,
    }

    pub(crate) struct DragMapDetailTokenEndEvent {
        pub pos: Point,
    }

    pub(crate) struct DragMapDetailTokenCancelEvent;

    pub(crate) struct ApplyMapDetailEvent;

    pub struct MapDetailController {
        view: MapDetailView,
    }

    impl MapDetailController {
        pub fn init(view: MapDetailView) {
            let controller = MapDetailController { view };
            let activity = nuts::new_domained_activity(controller, &DefaultDomain);

            // register private events
            activity.private_channel(|controller, event: AddTokenPropertiesEvent| {
                controller.view.add_token_properties(event.token_id);
            });
            activity.private_channel(|controller, event: DeleteTokenPropertiesEvent| {
                controller.view.delete_token_properties(event.index);
            });
            activity.private_channel(|controller, event: ToggleTokenPropertiesEvent| {
                controller.view.toggle_token_properties(event.index);
            });
            activity.private_channel(|controller, event: UpdateTokenTypeEvent| {
                controller.view.update_token_type(event.index, event.token_id);
            });
            activity.private_channel(|controller, event: UpdateOxygenVariantEvent| {
                controller.view.update_oxygen_variant(event.index, event.number);
            });
            activity.private_channel(|controller, event: UpdateTokenDistanceEvent| {
                controller.view.update_token_distance(event.index, event.value);
            });
            activity.private_channel(|controller, event: UpdateTokenAngleEvent| {
                controller.view.update_token_angle(event.index, event.value);
            });
            activity.private_channel(|controller, event: UpdateTokenOrientationEvent| {
                controller.view.update_token_orientation(event.index, event.value);
            });
            activity.private_channel(|controller, event: DragMapDetailTokenBeginEvent| {
                controller.view.drag_token_begin(event.index, event.pos);
            });
            activity.private_channel(|controller, event: DragMapDetailTokenMoveEvent| {
                controller.view.drag_token_move(event.pos);
            });
            activity.private_channel(|controller, event: DragMapDetailTokenEndEvent| {
                controller.view.drag_token_end(event.pos);
            });
            activity.private_channel(|controller, _event: DragMapDetailTokenCancelEvent| {
                controller.view.drag_token_cancel();
            });
            activity.private_channel(|controller, _event: ApplyMapDetailEvent| {
                controller.view.apply_map_detail();
            });

            // register public events
            activity.subscribe_domained(Self::show);
            activity.subscribe(Self::hide);
        }

        fn show(&mut self, domain: &mut DomainState, _event: &ShowMapDetailEvent) {
            if let Some(settings) = domain.try_get::<MapSettings>() {
                let center = match settings.selected {
                    Some(val) => val,
                    None => return,
                };
                self.view.update_background_grid(settings.background_grid_visible);
                self.view.update_map_tiles(&settings.map, center);
            }
            self.view.set_active(true);
        }

        fn hide(&mut self, _event: &HideMapDetailEvent) {
            self.view.set_active(false);
        }
    }
}

//----------------------------------------------------------------------------

pub mod export {
    use nuts::{DefaultDomain, DomainState};
    use super::*;

    pub(crate) struct DrawExportTileDoneEvent {
        pub tile: PlacedTile,
    }

    pub(crate) struct DrawExportTokenDoneEvent {
        pub tile: PlacedTile,
        pub token: PlacedToken,
    }

    pub struct ExportController {
        view: ExportView,
        storage: Storage,
    }

    impl ExportController {
        pub fn init(view: ExportView) {
            let storage = Storage::new("export");
            let controller = ExportController { view, storage };
            let activity = nuts::new_domained_activity(controller, &DefaultDomain);

            // register private events
            activity.private_channel(|controller, event| {
                controller.load_settings(&event);
            });
            activity.private_channel(|controller, event| {
                controller.save_settings(&event);
            });
            activity.private_channel(|controller, event: DrawExportTileDoneEvent| {
                controller.view.draw_export_tile_done(&event.tile);
            });
            activity.private_channel(|controller, event: DrawExportTokenDoneEvent| {
                controller.view.draw_export_token_done(&event.tile, &event.token);
            });

            // register public events
            activity.subscribe(ExportController::load_settings);
            activity.subscribe(ExportController::save_settings);
            activity.subscribe(ExportController::update_export_scale);
            activity.subscribe(ExportController::update_export_header);
            activity.subscribe(ExportController::update_export_listing);
            activity.subscribe(ExportController::update_export_color_scheme);
            activity.subscribe_domained(ExportController::export_image);
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

        fn update_export_scale(&mut self, event: &UpdateExportScaleEvent) {
            self.view.update_export_scale(event.scale);
        }

        fn update_export_header(&mut self, event: &UpdateExportHeaderEvent) {
            self.view.update_export_header(event.visible);
        }

        fn update_export_listing(&mut self, event: &UpdateExportListingEvent) {
            self.view.update_export_listing(event.visible);
        }

        fn update_export_color_scheme(&mut self, event: &UpdateExportColorSchemeEvent) {
            self.view.update_export_color_scheme(event.color_scheme);
        }

        fn export_image(&mut self, domain: &mut DomainState, _event: &ExportImageEvent) {
            if let Some(settings) = domain.try_get::<MapSettings>() {
                self.view.draw_export_image(&settings.map, settings.label_type);
            }
        }
    }
}

//----------------------------------------------------------------------------

pub mod welcome {
    use nuts::DefaultDomain;
    use super::*;

    pub struct ShowWelcomeEvent;

    pub struct HideWelcomeEvent;

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
            activity.subscribe(WelcomeController::drag_catalog_tile_end);
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

        fn drag_catalog_tile_end(&mut self, _event: &DragCatalogTileEndEvent) {
            self.view.set_hidden(true);
        }

        fn show_welcome(&mut self, _event: &ShowWelcomeEvent) {
            self.view.set_hidden(false);
        }

        fn hide_welcome(&mut self, _event: &HideWelcomeEvent) {
            self.view.set_hidden(true);
        }
    }
}

//----------------------------------------------------------------------------
