//----------------------------------------------------------------------------
//! Configuration of the available catalog tiles.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Document, Element};

use crate::check;
use crate::controller::UpdateCatalogEditionsEvent;
use crate::controller::catalog_config::*;
use crate::edition::{Edition, Series};
use super::*;

//----------------------------------------------------------------------------

struct EditionCard {
    inner: Element,
    edition: Edition,
    active: bool,
    icon: Element,
    toggle_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl EditionCard {
    fn new(document: &Document, edition: Edition) -> Result<Self> {
        let card = document.create_element("div")?;
        card.set_attribute("class", "card is-clickable is-unselectable")?;

        let card_content = document.create_element("div")?;
        card_content.set_attribute("class", "card-content")?;
        card.append_child(&card_content)?;

        let media = document.create_element("div")?;
        media.set_attribute("class", "media")?;
        card_content.append_child(&media)?;

        let media_left = document.create_element("div")?;
        media_left.set_attribute("class", "media-left")?;
        media.append_child(&media_left)?;

        let figure = document.create_element("figure")?;
        figure.set_attribute("class", "image is-48x48")?;
        media_left.append_child(&figure)?;

        let image = document.create_element("img")?;
        image.set_attribute("src", &format!("editions/thumb-{:x}.png", edition))?;
        image.set_attribute("alt", &edition.to_string())?;
        figure.append_child(&image)?;

        let media_content = document.create_element("div")?;
        media_content.set_attribute("class", "media-content")?;
        media.append_child(&media_content)?;

        let mut title_text;
        let subtitle_text;
        if edition.is_expansion() {
            title_text = edition.to_string();
            let offset = title_text.find(' ').unwrap_or(0);
            title_text.replace_range(0..offset, "");
            subtitle_text = "Expansion";
        } else {
            title_text = edition.series().to_string();
            subtitle_text = "Core Box";
        }

        let title = document.create_element("p")?;
        title.set_attribute("class", "title is-6")?;
        title.append_child(&document.create_text_node(&title_text))?;
        media_content.append_child(&title)?;

        let subtitle = document.create_element("p")?;
        subtitle.set_attribute("class", "subtitle is-6")?;
        subtitle.append_child(&document.create_text_node(subtitle_text))?;
        media_content.append_child(&subtitle)?;

        let media_right = document.create_element("div")?;
        media_right.set_attribute("class", "media-right")?;
        media.append_child(&media_right)?;

        let icon = document.create_element("span")?;
        icon.set_attribute("class", "icon has-text-grey")?;
        media_right.append_child(&icon)?;

        let icon_svg = document.create_element_ns(SVG_NS, "svg")?;
        icon_svg.set_attribute("class", "bi")?;
        icon_svg.set_attribute("width", "16")?;
        icon_svg.set_attribute("height", "16")?;
        icon_svg.set_attribute("fill", "currentColor")?;
        icon.append_child(&icon_svg)?;

        let icon_use = document.create_element_ns(SVG_NS, "use")?;
        icon_use.set_attribute("href", "bootstrap-icons.svg#circle")?;
        icon_svg.append_child(&icon_use)?;

        let toggle_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::send_to::<CatalogConfigController, _>(ToggleCatalogEditionEvent { edition });
        }) as Box<dyn Fn(_)>);
        card.add_event_listener_with_callback("click",
            toggle_cb.as_ref().unchecked_ref())?;

        Ok(EditionCard { inner: card, edition, active: false, icon, toggle_cb })
    }

    pub fn set_active(&mut self, value: bool) {
        debug!("edition {} set active={}", self.edition, value);
        let icon_use = check!(self.icon.query_selector("use").ok().flatten());
        if value {
            if self.edition.series() == Series::Gt {
                check!(self.inner.class_list().add_1("has-background-info").ok());
            } else {
                check!(self.inner.class_list().add_1("has-background-success").ok());
            }
            check!(self.inner.class_list().add_1("has-text-white").ok());
            check!(self.icon.class_list().remove_1("has-text-grey").ok());
        } else {
            check!(self.inner.class_list().remove_1("has-background-info").ok());
            check!(self.inner.class_list().remove_1("has-background-success").ok());
            check!(self.inner.class_list().remove_1("has-text-white").ok());
            check!(self.icon.class_list().add_1("has-text-grey").ok());
        }
        let icon_href = if value {
            "bootstrap-icons.svg#check-circle-fill"
        } else {
            "bootstrap-icons.svg#circle"
        };
        check!(icon_use.set_attribute("href", icon_href).ok());
        self.active = value;
    }
}

impl AsRef<Element> for EditionCard {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

impl Drop for EditionCard {
    fn drop(&mut self) {
        let _ = self.inner.remove_event_listener_with_callback("click",
            self.toggle_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------

pub struct CatalogConfigView {
    inner: Element,
    editions: Vec<EditionCard>,
    apply: Element,
    apply_cb: Closure<dyn Fn(web_sys::Event)>,
    close: Element,
    close_cb: Closure<dyn Fn(web_sys::Event)>,
    keydown_cb: Closure<dyn Fn(web_sys::KeyboardEvent)>,
}

impl CatalogConfigView {
    pub fn new(parent: Element) -> Result<Self> {
        let document = parent.owner_document().unwrap();
        let inner = parent;
        let mut editions = Vec::new();

        let columns = inner.query_selector(".columns")?
            .ok_or("Cannot find columns of catalog config element")?;

        let column = document.create_element("div")?;
        column.set_attribute("class", "column")?;
        columns.append_child(&column)?;

        let mut edition_list: Vec<_> = Edition::iter().collect();
        // Move discontinued editions to the end of the list
        edition_list.sort_by_key(|edition| edition.is_discontinued());

        for &edition in &edition_list {
            if edition.series() != Series::Gt {
                continue;
            }
            let card = EditionCard::new(&document, *edition)?;
            column.append_child(card.as_ref())?;
            editions.push(card);
        }

        let column = document.create_element("div")?;
        column.set_attribute("class", "column")?;
        columns.append_child(&column)?;

        for &edition in &edition_list {
            if edition.series() != Series::Dirt {
                continue;
            }
            let card = EditionCard::new(&document, *edition)?;
            column.append_child(card.as_ref())?;
            editions.push(card);
        }

        let apply = document.get_element_by_id("apply-catalog-config")
            .ok_or("Cannot find apply button of catalog config element")?;

        let apply_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::send_to::<CatalogConfigController, _>(ApplyCatalogEditionsEvent);
            nuts::publish(HideCatalogConfigEvent);
        }) as Box<dyn Fn(_)>);
        apply.add_event_listener_with_callback("click", apply_cb.as_ref().unchecked_ref())?;

        let close = inner.query_selector(".modal-close")?
            .ok_or("Cannot find close button of catalog config element")?;

        let close_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::publish(HideCatalogConfigEvent);
        }) as Box<dyn Fn(_)>);
        close.add_event_listener_with_callback("click", close_cb.as_ref().unchecked_ref())?;

        let keydown_cb = Closure::wrap(Box::new(move |event: web_sys::KeyboardEvent| {
            if event.repeat() {
                return;
            }
            if event.key().eq_ignore_ascii_case("enter") && (event.ctrl_key() || event.meta_key()) {
                nuts::send_to::<CatalogConfigController, _>(ApplyCatalogEditionsEvent);
                nuts::publish(HideCatalogConfigEvent);
            }
            if event.key().eq_ignore_ascii_case("escape") {
                nuts::publish(HideCatalogConfigEvent);
            }
        }) as Box<dyn Fn(_)>);

        Ok(CatalogConfigView {
            inner, editions, apply, apply_cb, close, close_cb, keydown_cb
        })
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

    pub fn set_editions(&mut self, editions: &[Edition]) {
        for card in self.editions.iter_mut() {
            card.set_active(editions.contains(&card.edition));
        }
    }

    pub fn toggle_edition(&mut self, edition: Edition) {
        debug!("toggle edition {}", edition);
        for card in self.editions.iter_mut() {
            if card.edition == edition {
                card.set_active(!card.active);
            }
        }
    }

    pub fn apply_catalog_editions(&mut self) {
        let editions = self.editions.iter()
            .filter(|card| card.active)
            .map(|card| card.edition)
            .collect();
        nuts::publish(UpdateCatalogEditionsEvent { editions });
    }
}

impl Drop for CatalogConfigView {
    fn drop(&mut self) {
        let document = self.inner.owner_document().unwrap();

        let _ = self.apply.remove_event_listener_with_callback("click",
            self.apply_cb.as_ref().unchecked_ref());
        let _ = self.close.remove_event_listener_with_callback("click",
            self.close_cb.as_ref().unchecked_ref());
        let _ = document.remove_event_listener_with_callback("keydown",
            self.keydown_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------
