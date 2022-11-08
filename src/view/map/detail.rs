//----------------------------------------------------------------------------
//! Display of map details and configuration of track tokens.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{self, Document, Element, Node};

use crate::check;
use crate::controller::*;
use super::*;
use super::polar::PolarCoordinate;

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

fn sanitize_input_value(value: f64, min: f64, max: f64, step: f64) -> f64 {
    if value <= min {
        min
    } else if value >= max {
        max
    } else {
        (value / step).round() * step
    }
}

//----------------------------------------------------------------------------

struct TokenTypeField {
    inner: Element,
    input: web_sys::HtmlSelectElement,
    change_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl TokenTypeField {
    fn new(document: &Document, token_id: TokenId, index: u32) -> Result<Self> {
        let field = document.create_element("div")?;
        field.set_attribute("class", "field")?;

        let control = document.create_element("div")?;
        control.set_attribute("class", "control is-expanded")?;
        field.append_child(&control)?;

        let select = document.create_element("div")?;
        select.set_attribute("class", "select is-fullwidth")?;
        control.append_child(&select)?;

        let input = document.create_element("select")?
            .dyn_into::<web_sys::HtmlSelectElement>().unwrap();
        input.set_attribute("name", "token-type")?;
        select.append_child(&input)?;

        let change_cb = Closure::wrap(Box::new(move |event: web_sys::Event| {
            let input = check!(event.target()
                .and_then(|target| target.dyn_into::<web_sys::HtmlSelectElement>().ok()));
            let token_id: TokenId = check!(input.value().parse().ok());
            nuts::send_to::<MapDetailController, _>(UpdateTokenTypeEvent { index, token_id });
        }) as Box<dyn Fn(_)>);
        input.add_event_listener_with_callback("change",
            change_cb.as_ref().unchecked_ref()).unwrap();

        for &id in TokenId::iter() {
            let option = document.create_element("option")?;
            option.set_attribute("value", &id.to_string())?;
            if id == token_id.base() {
                option.set_attribute("selected", "selected")?;
            }
            input.append_child(&option)?;
            let text = id.to_string();
            option.append_child(&document.create_text_node(&text))?;
        }

        Ok(TokenTypeField { inner: field, input, change_cb })
    }

    fn set_value(&self, value: TokenId) {
        let text = value.base().to_string();
        self.input.set_value(&text);
    }
}

impl AsRef<Element> for TokenTypeField {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

impl Drop for TokenTypeField {
    fn drop(&mut self) {
        let _ = self.input.remove_event_listener_with_callback("change",
            self.change_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------

struct OxygenVariantField {
    inner: Element,
    value: u8,
    inputs: Vec<web_sys::HtmlInputElement>,
    change_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl OxygenVariantField {
    fn new(document: &Document, token_id: TokenId, index: u32) -> Result<Self> {
        let value = match token_id {
            TokenId::Oxygen(0) => 1,
            TokenId::Oxygen(val) => val,
            _ => 1,
        };

        let field = document.create_element("div")?;
        field.set_attribute("class", "field")?;
        field.set_hidden(!matches!(token_id, TokenId::Oxygen(_)));

        let control = document.create_element("div")?;
        control.set_attribute("class", "control is-expanded")?;
        field.append_child(&control)?;

        let legend = document.create_element("span")?;
        legend.set_attribute("class", "legend")?;
        legend.append_child(&document.create_text_node("Variant: "))?;
        control.append_child(&legend)?;

        let change_cb = Closure::wrap(Box::new(move |event: web_sys::Event| {
            let input = check!(event.target()
                .and_then(|target| target.dyn_into::<web_sys::HtmlInputElement>().ok()));
            let number: u8 = check!(input.value().parse().ok());
            nuts::send_to::<MapDetailController, _>(UpdateOxygenVariantEvent { index, number });
        }) as Box<dyn Fn(_)>);

        let mut inputs = Vec::with_capacity(3);
        for number in 1..=3_u8 {
            let label = document.create_element("label")?;
            label.set_attribute("class", "radio")?;
            control.append_child(&label)?;

            let input = document.create_element("input")?
                .dyn_into::<web_sys::HtmlInputElement>().unwrap();
            input.set_attribute("name", "variant")?;
            input.set_attribute("value", &number.to_string())?;
            if number == value {
                input.set_attribute("checked", "checked")?;
            }
            input.set_type("radio");
            label.append_child(&input)?;
            let text = format!("-{}", number);
            label.append_child(&document.create_text_node(&text))?;

            input.add_event_listener_with_callback("input",
                change_cb.as_ref().unchecked_ref()).unwrap();
            inputs.push(input);
        }

        Ok(OxygenVariantField { inner: field, value, inputs, change_cb })
    }

    fn value(&self) -> u8 {
        self.value
    }

    fn set_value(&mut self, value: u8) {
        self.value = value;
        for input in &self.inputs {
            if let Ok(number) = input.value().parse::<u8>() {
                input.set_checked(number == value);
            }
        }
    }
}

impl AsRef<Element> for OxygenVariantField {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

impl Drop for OxygenVariantField {
    fn drop(&mut self) {
        for input in &self.inputs {
            let _ = input.remove_event_listener_with_callback("input",
                self.change_cb.as_ref().unchecked_ref());
        }
    }
}

//----------------------------------------------------------------------------

struct TokenSliderInputField {
    inner: Element,
    min: f64,
    max: f64,
    step: f64,
    precision: usize,
    input: web_sys::HtmlInputElement,
    number: web_sys::HtmlInputElement,
    change_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl TokenSliderInputField {
    fn inner_new(document: &Document, name: &str, min: f64, max: f64, step: f64, precision: usize, update_cb: Box<dyn Fn(f32)>) -> Result<Self> {
        let field = document.create_element("div")?;
        field.set_attribute("class", "field")?;

        let control = document.create_element("div")?;
        control.set_attribute("class", "control")?;
        field.append_child(&control)?;

        let label = document.create_element("label")?;
        label.set_attribute("class", "slider")?;
        control.append_child(&label)?;

        let label_span = document.create_element("span")?;
        let mut text = String::from(name);
        if !text.is_empty() {
            text[0..1].make_ascii_uppercase();
            text.push(':');
        }
        label_span.append_child(&document.create_text_node(&text))?;
        label.append_child(&label_span)?;

        let min_str = format!("{:.*}", precision, min);
        let max_str = format!("{:.*}", precision, max);
        let step_str = format!("{:.*}", precision, step);

        let input = document.create_element("input")?
            .dyn_into::<web_sys::HtmlInputElement>().unwrap();
        input.set_attribute("name", name)?;
        input.set_type("range");
        input.set_min(&min_str);
        input.set_max(&max_str);
        input.set_step(&step_str);
        label.append_child(&input)?;

        let number = document.create_element("input")?
            .dyn_into::<web_sys::HtmlInputElement>().unwrap();
        // convince the browser to use the same number formatting as we do
        number.set_attribute("lang", "en-US")?;
        number.set_type("number");
        number.set_min(&min_str);
        number.set_max(&max_str);
        number.set_step(&step_str);
        label.append_child(&number)?;

        let change_cb = Closure::wrap(Box::new(move |event: web_sys::Event| {
            let input = check!(event.target()
                .and_then(|target| target.dyn_into::<web_sys::HtmlInputElement>().ok()));
            if input.check_validity() || event.type_() == "blur" {
                let value = sanitize_input_value(input.value_as_number(), min, max, step);
                update_cb(value as f32);
            }
        }) as Box<dyn Fn(_)>);

        input.add_event_listener_with_callback("input",
            change_cb.as_ref().unchecked_ref()).unwrap();
        number.add_event_listener_with_callback("input",
            change_cb.as_ref().unchecked_ref()).unwrap();
        // sanitize the user input value after control has lost focus
        number.add_event_listener_with_callback("blur",
            change_cb.as_ref().unchecked_ref()).unwrap();

        Ok(TokenSliderInputField { inner: field, min, max, step, precision, input, number, change_cb })
    }

    fn new_token_distance(document: &Document, value: f32, index: u32) -> Result<Self> {
        let update_cb = Box::new(move |value: f32| {
            nuts::send_to::<MapDetailController, _>(UpdateTokenDistanceEvent { index, value });
        });

        let field = Self::inner_new(document, "distance", 0.0, 1.0, 0.01, 2, update_cb)?;
        field.set_value(value);
        Ok(field)
    }

    fn new_token_angle(document: &Document, value: f32, index: u32) -> Result<Self> {
        let update_cb = Box::new(move |value: f32| {
            nuts::send_to::<MapDetailController, _>(UpdateTokenAngleEvent { index, value });
        });

        let field = Self::inner_new(document, "angle", 0.0, 360.0, 1.0, 0, update_cb)?;
        field.set_value(value.rem_euclid(360.0));
        Ok(field)
    }

    fn new_token_orientation(document: &Document, value: f32, index: u32) -> Result<Self> {
        let update_cb = Box::new(move |value: f32| {
            nuts::send_to::<MapDetailController, _>(UpdateTokenOrientationEvent { index, value });
        });

        let field = Self::inner_new(document, "orientation", 0.0, 360.0, 1.0, 0, update_cb)?;
        field.set_value(value.rem_euclid(360.0));
        Ok(field)
    }

    fn value(&self) -> f32 {
        self.input.value_as_number() as f32
    }

    fn set_value(&self, value: f32) {
        // sanitize value, otherwise the input element will be flagged "invalid"
        let value = sanitize_input_value(f64::from(value), self.min, self.max, self.step);

        // update the value as a string, this allows us to control the numeric precision
        let value_str = format!("{:.*}", self.precision, value);
        let document = self.inner.owner_document().unwrap();
        if Some(self.input.as_ref()) != document.active_element().as_ref() {
            self.input.set_value(&value_str);
        }
        if Some(self.number.as_ref()) != document.active_element().as_ref() {
            self.number.set_value(&value_str);
        }
    }
}

impl AsRef<Element> for TokenSliderInputField {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

impl Drop for TokenSliderInputField {
    fn drop(&mut self) {
        let _ = self.input.remove_event_listener_with_callback("input",
            self.change_cb.as_ref().unchecked_ref());
        let _ = self.number.remove_event_listener_with_callback("input",
            self.change_cb.as_ref().unchecked_ref());
        let _ = self.number.remove_event_listener_with_callback("blur",
            self.change_cb.as_ref().unchecked_ref());
    }
}

//----------------------------------------------------------------------------

struct TokenImageProperties {
    inner: Element,
    layout: Layout,
    tile: PlacedTile,
    token: PlacedToken,
    index: u32,
    active: bool,
    card_header: Element,
    card_header_title: Element,
    card_content: Element,
    card_footer: Element,
    token_type: TokenTypeField,
    oxygen_variant: OxygenVariantField,
    token_distance: TokenSliderInputField,
    token_angle: TokenSliderInputField,
    token_orientation: TokenSliderInputField,
    image: TokenImageElement,
    toggle_icon: Element,
    toggle_cb: Closure<dyn Fn(web_sys::Event)>,
    delete: Element,
    delete_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl TokenImageProperties {
    fn new(document: &Document, layout: Layout, tile: PlacedTile, token: PlacedToken, index: u32, image_parent: &Element) -> Result<Self> {
        let active = index == 0;

        let image = TokenImageElement::new(document, &layout, &tile, &token)?;
        image_parent.append_child(&image)?;

        let card = document.create_element("div")?;
        card.set_attribute("class", "card")?;

        let card_header = document.create_element("header")?;
        card_header.set_attribute("class", "card-header is-clickable")?;
        card.append_child(&card_header)?;

        let card_header_title = document.create_element("p")?;
        card_header_title.set_attribute("class", "card-header-title")?;
        let text = format!("Token: {}", token.id.base());
        card_header_title.append_child(&document.create_text_node(&text))?;
        card_header.append_child(&card_header_title)?;

        let card_header_icon = document.create_element("div")?;
        card_header_icon.set_attribute("class", "card-header-icon")?;
        card_header.append_child(&card_header_icon)?;

        let toggle_icon = document.create_element("span")?;
        toggle_icon.set_attribute("class", "icon")?;
        card_header_icon.append_child(&toggle_icon)?;

        let icon_svg = document.create_element_ns(SVG_NS, "svg")?;
        icon_svg.set_attribute("class", "bi")?;
        icon_svg.set_attribute("width", "16")?;
        icon_svg.set_attribute("height", "16")?;
        icon_svg.set_attribute("fill", "currentColor")?;
        toggle_icon.append_child(&icon_svg)?;

        let icon_use = document.create_element_ns(SVG_NS, "use")?;
        icon_use.set_attribute("href", "bootstrap-icons.svg#chevron-down")?;
        icon_svg.append_child(&icon_use)?;

        let card_content = document.create_element("div")?;
        card_content.set_attribute("class", "card-content")?;
        card.append_child(&card_content)?;

        let token_type = TokenTypeField::new(document, token.id, index)?;
        card_content.append_child(token_type.as_ref())?;

        let oxygen_variant = OxygenVariantField::new(document, token.id, index)?;
        oxygen_variant.set_hidden(!matches!(token.id, TokenId::Oxygen(_)));
        card_content.append_child(oxygen_variant.as_ref())?;

        let polar = PolarCoordinate::from_coordinate(token.pos);
        let orientation = token.dir.to_angle() - polar.angle();

        let token_distance = TokenSliderInputField::new_token_distance(document, polar.distance(), index)?;
        card_content.append_child(token_distance.as_ref())?;

        let token_angle = TokenSliderInputField::new_token_angle(document, polar.angle(), index)?;
        card_content.append_child(token_angle.as_ref())?;

        let token_orientation = TokenSliderInputField::new_token_orientation(document, orientation, index)?;
        card_content.append_child(token_orientation.as_ref())?;

        let card_footer = document.create_element("footer")?;
        card_footer.set_attribute("class", "card-footer")?;
        card.append_child(&card_footer)?;

        let delete = document.create_element("a")?;
        delete.set_attribute("class", "card-footer-item is-danger")?;
        delete.append_child(&document.create_text_node("Delete Token"))?;
        card_footer.append_child(&delete)?;

        let toggle_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::send_to::<MapDetailController, _>(ToggleTokenPropertiesEvent { index });
        }) as Box<dyn Fn(_)>);
        card_header.add_event_listener_with_callback("click",
            toggle_cb.as_ref().unchecked_ref())?;

        let delete_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::send_to::<MapDetailController, _>(DeleteTokenPropertiesEvent { index });
        }) as Box<dyn Fn(_)>);
        delete.add_event_listener_with_callback("click",
            delete_cb.as_ref().unchecked_ref())?;

        let mut item = TokenImageProperties {
            inner: card, layout, tile, token, index, active, image, card_header,
            card_header_title, card_content, card_footer, token_type,
            oxygen_variant, token_distance, token_angle, token_orientation,
            toggle_icon, toggle_cb, delete, delete_cb
        };
        item.set_active(active);
        Ok(item)
    }

    pub fn set_active(&mut self, value: bool) {
        debug!("token properties #{} set active={}", self.index, value);
        self.card_content.set_hidden(!value);
        self.card_footer.set_hidden(!value);
        self.toggle_icon.set_hidden(value);
        self.active = value;
    }

    pub fn set_token_type(&mut self, token_id: TokenId) {
        debug!("token properties #{} set token type={}", self.index, token_id);
        let terrain = self.tile.terrain().unwrap_or_default();
        let token_id = if token_id == TokenId::Oxygen(0) {
            // choose a default oxygen level
            TokenId::Oxygen(self.oxygen_variant.value())
        } else {
            token_id.with_terrain(terrain)
        };
        if token_id != self.token.id {
            self.token.id = token_id;
            let text = format!("Token: {}", token_id.base());
            self.card_header_title.set_inner_html(&text);
            self.token_type.set_value(token_id);
            match token_id {
                TokenId::Oxygen(val) => {
                    self.oxygen_variant.set_hidden(false);
                    self.oxygen_variant.set_value(val);
                },
                _ => {
                    self.oxygen_variant.set_hidden(true);
                },
            }
            self.image.set_token(&self.layout, &self.tile, &self.token);
        }
    }

    pub fn set_oxygen_variant(&mut self, number: u8) {
        debug!("token properties #{} set oxygen variant={}", self.index, number);
        let token_id = self.token.id.with_number(number);
        if token_id != self.token.id {
            self.token.id = token_id;
            let text = format!("Token: {}", token_id.base());
            self.card_header_title.set_inner_html(&text);
            self.oxygen_variant.set_value(number);
            self.image.set_token(&self.layout, &self.tile, &self.token);
        }
    }

    pub fn set_token_distance(&mut self, value: f32) {
        debug!("token properties #{} set token distance={}", self.index, value);
        let angle = self.token_angle.value();
        self.token.pos = PolarCoordinate::new(value, angle).to_coordinate();
        self.token_distance.set_value(value);
        self.image.set_token(&self.layout, &self.tile, &self.token);
    }

    pub fn set_token_angle(&mut self, value: f32) {
        debug!("token properties #{} set token angle={}", self.index, value);
        let distance = self.token_distance.value();
        self.token.pos = PolarCoordinate::new(distance, value).to_coordinate();
        let orientation = self.token_orientation.value();
        self.token.dir = FloatDirection::from_angle(orientation + value);
        self.token_angle.set_value(value);
        self.image.set_token(&self.layout, &self.tile, &self.token);
    }

    pub fn set_token_orientation(&mut self, value: f32) {
        debug!("token properties #{} set token orientation={}", self.index, value);
        let angle = self.token_angle.value();
        self.token.dir = FloatDirection::from_angle(value + angle);
        self.token_orientation.set_value(value);
        self.image.set_token(&self.layout, &self.tile, &self.token);
    }
}

impl AsRef<Element> for TokenImageProperties {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

impl Drop for TokenImageProperties {
    fn drop(&mut self) {
        let _ = self.card_header.remove_event_listener_with_callback("click",
            self.toggle_cb.as_ref().unchecked_ref());
        let _ = self.delete.remove_event_listener_with_callback("click",
            self.delete_cb.as_ref().unchecked_ref());
        self.image.remove();
        self.inner.remove();
    }
}

//----------------------------------------------------------------------------

struct TokenImagePropertiesAdder {
    inner: Element,
    add: Element,
    add_cb: Closure<dyn Fn(web_sys::Event)>,
}

impl TokenImagePropertiesAdder {
    fn new(document: &Document, index: u32) -> Result<Self> {
        let card = document.create_element("div")?;
        card.set_attribute("class", "card")?;

        let card_content = document.create_element("div")?;
        card_content.set_attribute("class", "card-content")?;
        card.append_child(&card_content)?;

        let token_type = TokenTypeField::new(document, TokenId::default(), index)?;
        card_content.append_child(token_type.as_ref())?;

        let card_footer = document.create_element("footer")?;
        card_footer.set_attribute("class", "card-footer")?;
        card.append_child(&card_footer)?;

        let add = document.create_element("a")?;
        add.set_attribute("class", "card-footer-item is-link")?;
        add.append_child(&document.create_text_node("Add Token"))?;
        card_footer.append_child(&add)?;

        let add_cb = Closure::wrap(Box::new({
            let input = token_type.input.clone();
            move |_event: web_sys::Event| {
                let token_id: TokenId = check!(input.value().parse().ok());
                nuts::send_to::<MapDetailController, _>(AddTokenPropertiesEvent { token_id });
            }
        }) as Box<dyn Fn(_)>);
        add.add_event_listener_with_callback("click",
            add_cb.as_ref().unchecked_ref())?;

        Ok(TokenImagePropertiesAdder { inner: card, add, add_cb })
    }
}

impl AsRef<Element> for TokenImagePropertiesAdder {
    fn as_ref(&self) -> &Element {
        &self.inner
    }
}

impl AsRef<Node> for TokenImagePropertiesAdder {
    fn as_ref(&self) -> &Node {
        &self.inner
    }
}

impl Drop for TokenImagePropertiesAdder {
    fn drop(&mut self) {
        let _ = self.add.remove_event_listener_with_callback("click",
            self.add_cb.as_ref().unchecked_ref());
        self.inner.remove();
    }
}

//----------------------------------------------------------------------------

pub struct MapDetailView {
    inner: Element,
    layout: Layout,
    tile_layout: Layout,
    center_tile: Option<PlacedTile>,
    grid: Element,
    tiles: Element,
    tokens: Element,
    token_properties: Vec<TokenImageProperties>,
    token_properties_adder: TokenImagePropertiesAdder,
    token_properties_column: Element,
    apply: Element,
    apply_cb: Closure<dyn Fn(web_sys::Event)>,
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
        let tile_layout = layout.clone();
        let center_tile = None;

        let token_properties = Vec::new();

        let columns = inner.query_selector(".columns")?
            .ok_or("Cannot find columns of map detail element")?;

        let map_detail_column = document.create_element("div")?;
        map_detail_column.set_attribute("class", "column")?;
        columns.append_child(&map_detail_column)?;

        let token_properties_column = document.create_element("div")?;
        token_properties_column.set_attribute("class", "column")?;
        columns.append_child(&token_properties_column)?;

        let token_properties_adder = TokenImagePropertiesAdder::new(&document, 0)?;
        token_properties_column.append_child(token_properties_adder.as_ref())?;

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
        map_detail_column.append_child(&canvas)?;

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

        let apply_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::send_to::<MapDetailController, _>(ApplyMapDetailEvent);
            nuts::publish(HideMapDetailEvent);
        }) as Box<dyn Fn(_)>);
        apply.add_event_listener_with_callback("click", apply_cb.as_ref().unchecked_ref())?;

        let close = inner.query_selector(".modal-close")?
            .ok_or("Cannot find close button of map detail element")?;

        let close_cb = Closure::wrap(Box::new(move |_event: web_sys::Event| {
            nuts::publish(HideMapDetailEvent);
        }) as Box<dyn Fn(_)>);
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

        Ok(MapDetailView {
            inner, layout, tile_layout, center_tile, grid, tiles, tokens,
            token_properties, token_properties_adder, token_properties_column,
            apply, apply_cb, close, close_cb, keydown_cb
        })
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
        self.token_properties.clear();

        let offset = self.layout.origin() - center.to_pixel(&self.layout);
        self.tile_layout = self.layout.with_origin(self.layout.origin() + offset);

        // draw selected tile
        if let Some(tile) = map.get(center) {
            if let Ok(el) = TileImageElement::new(&document, &self.tile_layout, tile) {
                self.tiles.append_child(&el).unwrap();
            }
            let mut index = 1;
            for token in &tile.tokens {
                if let Ok(item) = TokenImageProperties::new(&document, self.tile_layout.clone(), tile.clone(), token.clone(), index, &self.tokens) {
                    self.token_properties_column.append_child(item.as_ref()).unwrap();
                    self.token_properties.push(item);
                    index += 1;
                }
            }
            self.center_tile = Some(tile.clone());
        } else {
            self.center_tile = None;
        }

        // move token properties adder at the end of the column
        if let Some(item) = self.token_properties.last() {
            let item: &Element = item.as_ref();
            check!(item.after_with_node_1(self.token_properties_adder.as_ref()).ok());
        }

        // draw tiles around the selected tile
        for &dir in Direction::iter() {
            let pos = center.neighbor(dir);
            if let Some(tile) = map.get(pos) {
                if let Ok(el) = TileImageElement::new(&document, &self.tile_layout, tile) {
                    self.tiles.append_child(&el).unwrap();
                }
                for token in &tile.tokens {
                    if let Ok(el) = TokenImageElement::new(&document, &self.tile_layout, tile, token) {
                        self.tokens.append_child(&el).unwrap();
                    }
                }
            }
        }

        if let Some(item) = self.token_properties.first() {
            self.toggle_token_properties(item.index);
        }
    }

    fn get_token_properties_mut(&mut self, index: u32) -> Option<&mut TokenImageProperties> {
        self.token_properties.iter_mut().find(|item| item.index == index)
    }

    pub fn update_token_type(&mut self, index: u32, token_id: TokenId) {
        if let Some(item) = self.get_token_properties_mut(index) {
            item.set_token_type(token_id);
        }
    }

    pub fn update_oxygen_variant(&mut self, index: u32, number: u8) {
        if let Some(item) = self.get_token_properties_mut(index) {
            item.set_oxygen_variant(number);
        }
    }

    pub fn update_token_distance(&mut self, index: u32, value: f32) {
        if let Some(item) = self.get_token_properties_mut(index) {
            item.set_token_distance(value);
        }
    }

    pub fn update_token_angle(&mut self, index: u32, value: f32) {
        if let Some(item) = self.get_token_properties_mut(index) {
            item.set_token_angle(value);
        }
    }

    pub fn update_token_orientation(&mut self, index: u32, value: f32) {
        if let Some(item) = self.get_token_properties_mut(index) {
            item.set_token_orientation(value);
        }
    }

    pub fn add_token_properties(&mut self, token_id: TokenId) {
        let document = self.tokens.owner_document().unwrap();
        let tile = check!(self.center_tile.clone());
        let token_id = if token_id == TokenId::Oxygen(0) {
            TokenId::Oxygen(1)
        } else {
            let terrain = tile.terrain().unwrap_or_default();
            token_id.with_terrain(terrain)
        };
        let token = PlacedToken::new(token_id, FloatCoordinate::default(), FloatDirection::default());
        let index = self.token_properties.iter()
            .map(|item| item.index)
            .max()
            .unwrap_or(0) + 1;

        let item = check!(TokenImageProperties::new(&document, self.tile_layout.clone(), tile, token, index, &self.tokens).ok());
        let token_properties_adder: &Element = self.token_properties_adder.as_ref();
        check!(token_properties_adder.before_with_node_1(item.as_ref()).ok());
        self.token_properties.push(item);
        self.toggle_token_properties(index);
    }

    pub fn delete_token_properties(&mut self, index: u32) {
        self.token_properties.retain(|item| item.index != index);
    }

    pub fn toggle_token_properties(&mut self, index: u32) {
        for item in self.token_properties.iter_mut() {
            item.set_active(item.index == index);
        }
    }

    pub fn apply_map_detail(&mut self) {
        let tokens: Vec<PlacedToken> = self.token_properties.iter()
            .map(|item| item.token.clone())
            .collect();
        nuts::publish(UpdateSelectedTileTokensEvent { tokens });
    }
}

impl Drop for MapDetailView {
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
