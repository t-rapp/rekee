//----------------------------------------------------------------------------
//! Token which can be added to tiles. Can be used to add effects like a jump or
//! water hazard area to a track tile. They are also used to indicate entrance
//! (start) or exit of a joker section in rally-cross (RX) circuits.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use std::fmt;
use std::str::FromStr;

use serde::{Serialize, Serializer, Deserialize};

use crate::tile::Terrain;

//----------------------------------------------------------------------------

/// Identifier for a token which can be placed on tiles.
///
/// # Examples
///
/// ```
/// # use rekee::token::*;
/// # use rekee::tile::Terrain;
/// let token: TokenId = "chicane".parse().unwrap();
/// assert_eq!(token, TokenId::Chicane(Terrain::None));
///
/// let token = TokenId::Jump(Terrain::Gravel);
/// assert_eq!(token.to_string(), "Jump Gravel");
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Deserialize)]
#[serde(try_from = "&str")]
pub enum TokenId {
    /// Chicane token without a speed limit, part of the Rallyman DIRT Copilot Pack.
    Chicane(Terrain),
    /// Chicane token with a speed limit, part of the Rallyman DIRT Copilot Pack.
    ChicaneWithLimit(Terrain),
    /// Jump token, part of the Rallyman DIRT Copilot Pack.
    Jump(Terrain),
    /// Water token, part of the Rallyman DIRT Copilot Pack.
    Water(Terrain),
    /// Joker entrance arch, part of the RX expansion for Rallyman DIRT.
    JokerEntrance,
    /// Joker exit arch, part of the RX expansion for Rallyman DIRT.
    JokerExit,
    /// Finishing line arch, part of the RX expansion for Rallyman DIRT.
    Finish,
}

impl TokenId {
    /// Creates a copy of the current token with inner terrain changed to a
    /// given value.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::token::*;
    /// # use rekee::tile::Terrain;
    /// let token = TokenId::Chicane(Terrain::Asphalt);
    /// let other_token = token.with_terrain(Terrain::Snow);
    /// assert_eq!(other_token, TokenId::Chicane(Terrain::Snow));
    /// ```
    pub fn with_terrain(&self, terrain: Terrain) -> Self {
        match self {
            TokenId::Chicane(_) =>
                TokenId::Chicane(terrain),
            TokenId::ChicaneWithLimit(_) =>
                TokenId::ChicaneWithLimit(terrain),
            TokenId::Jump(_) =>
                TokenId::Jump(terrain),
            TokenId::Water(_) =>
                TokenId::Water(terrain),
            TokenId::JokerEntrance =>
                TokenId::JokerEntrance,
            TokenId::JokerExit =>
                TokenId::JokerExit,
            TokenId::Finish =>
                TokenId::Finish,
        }
    }

    /// Iterator over all track tokens.
    ///
    /// Tokens with terrain use the neutral `Terrain::None` value.
    ///
    /// # Examples
    ///
    /// ```
    /// # use rekee::token::*;
    /// let tokens: Vec<String> = TokenId::iter()
    ///     .map(|val| val.to_string())
    ///     .collect();
    /// assert_eq!(tokens, vec![
    ///     "Chicane",
    ///     "Chicane Limit",
    ///     "Jump",
    ///     "Water",
    ///     "Joker Entrance",
    ///     "Joker Exit",
    ///     "Finish"]);
    /// ```
    pub fn iter() -> std::slice::Iter<'static, Self> {
        const TOKENS: [TokenId; 7] = [
            TokenId::Chicane(Terrain::None),
            TokenId::ChicaneWithLimit(Terrain::None),
            TokenId::Jump(Terrain::None),
            TokenId::Water(Terrain::None),
            TokenId::JokerEntrance,
            TokenId::JokerExit,
            TokenId::Finish,
        ];
        TOKENS.iter()
    }
}

impl fmt::Display for TokenId {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut terrain = Terrain::None;
        match self {
            TokenId::Chicane(val) => {
                terrain = *val;
                write!(fmt, "Chicane")?;
            },
            TokenId::ChicaneWithLimit(val) => {
                terrain = *val;
                write!(fmt, "Chicane Limit")?;
            },
            TokenId::Jump(val) => {
                terrain = *val;
                write!(fmt, "Jump")?;
            },
            TokenId::Water(val) => {
                terrain = *val;
                write!(fmt, "Water")?;
            },
            TokenId::JokerEntrance =>
                write!(fmt, "Joker Entrance")?,
            TokenId::JokerExit =>
                write!(fmt, "Joker Exit")?,
            TokenId::Finish =>
                write!(fmt, "Finish")?,
        };
        if terrain != Terrain::None {
            write!(fmt, " {}", terrain)?;
        }
        Ok(())
    }
}

// small hack to provide the Serde string serialization as a formatter
impl fmt::LowerHex for TokenId {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut text = serde_json::to_string(self).unwrap();
        text.retain(|ch| ch != '"');
        write!(fmt, "{}", text)
    }
}

impl FromStr for TokenId {
    type Err = ParseTokenError;

    fn from_str(val: &str) -> Result<Self, Self::Err> {
        // match strings from both trait implementations, std::fmt::Display and serde::Serialize
        let mut s = val.replace(char::is_whitespace, "-");
        s.make_ascii_lowercase();

        let mut terrain = Terrain::None;
        let mut name = s.as_ref();
        if let Some((prefix, suffix)) = s.rsplit_once('-') {
            if let Ok(val) = suffix.parse() {
                terrain = val;
                name = prefix;
            }
        }
        match name {
            "chicane" =>
                Ok(TokenId::Chicane(terrain)),
            "chicane-limit" =>
                Ok(TokenId::ChicaneWithLimit(terrain)),
            "jump" =>
                Ok(TokenId::Jump(terrain)),
            "water" =>
                Ok(TokenId::Water(terrain)),
            "joker-entrance" =>
                Ok(TokenId::JokerEntrance),
            "joker-exit" =>
                Ok(TokenId::JokerExit),
            "finish" =>
                Ok(TokenId::Finish),
            _ =>
                Err(ParseTokenError::UnknownName(name.to_string())),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ParseTokenError {
    UnknownName(String),
}

impl fmt::Display for ParseTokenError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseTokenError::UnknownName(val) =>
                write!(fmt, "Unknown token name \"{}\"", val),
        }
    }
}

impl std::convert::TryFrom<&str> for TokenId {
    type Error = ParseTokenError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        TokenId::from_str(value)
    }
}

impl Serialize for TokenId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer
    {
        let mut data = self.to_string()
            .replace(char::is_whitespace, "-");
        data.make_ascii_lowercase();
        serializer.serialize_str(&data)
    }
}

//----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_to_str() {
        assert_eq!(TokenId::Chicane(Terrain::None).to_string(), "Chicane");
        assert_eq!(TokenId::ChicaneWithLimit(Terrain::Asphalt).to_string(), "Chicane Limit Asphalt");
        assert_eq!(TokenId::Jump(Terrain::Gravel).to_string(), "Jump Gravel");
        assert_eq!(TokenId::Water(Terrain::Snow).to_string(), "Water Snow");
        assert_eq!(TokenId::JokerEntrance.to_string(), "Joker Entrance");
        assert_eq!(TokenId::JokerExit.to_string(), "Joker Exit");
        assert_eq!(TokenId::Finish.to_string(), "Finish");

        let text = format!("{:x}", TokenId::Chicane(Terrain::Gravel));
        assert_eq!(text, "chicane-gravel");
        let text = format!("{:x}", TokenId::Jump(Terrain::None));
        assert_eq!(text, "jump");
        let text = format!("{:x}", TokenId::JokerExit);
        assert_eq!(text, "joker-exit");
    }

    #[test]
    fn token_from_str() {
        assert_eq!("chicane".parse::<TokenId>(), Ok(TokenId::Chicane(Terrain::None)));
        assert_eq!("chicane-limit-asphalt".parse::<TokenId>(), Ok(TokenId::ChicaneWithLimit(Terrain::Asphalt)));
        assert_eq!("Jump Gravel".parse::<TokenId>(), Ok(TokenId::Jump(Terrain::Gravel)));
        assert_eq!("WATER SNOW".parse::<TokenId>(), Ok(TokenId::Water(Terrain::Snow)));
        assert_eq!("Joker-Entrance".parse::<TokenId>(), Ok(TokenId::JokerEntrance));
        assert_eq!("jOKER-eXIT".parse::<TokenId>(), Ok(TokenId::JokerExit));
        assert_eq!("FiNiSh".parse::<TokenId>(), Ok(TokenId::Finish));

        assert!("".parse::<TokenId>().is_err());
        assert!("jump-4".parse::<TokenId>().is_err());
        assert!("joker".parse::<TokenId>().is_err());
        assert!("Asphalt".parse::<TokenId>().is_err());
    }

    #[test]
    fn token_serde() {
        let token = TokenId::Chicane(Terrain::Gravel);
        let text = serde_json::to_string(&token).unwrap();
        assert_eq!(text, r#""chicane-gravel""#);

        let token = TokenId::ChicaneWithLimit(Terrain::None);
        let text = serde_json::to_string(&token).unwrap();
        assert_eq!(text, r#""chicane-limit""#);

        let token = TokenId::Jump(Terrain::Asphalt);
        let text = serde_json::to_string(&token).unwrap();
        assert_eq!(text, r#""jump-asphalt""#);

        let text = r#""water-snow""#;
        let token: TokenId = serde_json::from_str(text).unwrap();
        assert_eq!(token, TokenId::Water(Terrain::Snow));

        let text = r#""joker-exit""#;
        let token: TokenId = serde_json::from_str(text).unwrap();
        assert_eq!(token, TokenId::JokerExit);

        let text = r#""#;
        let result: Result<TokenId, _> = serde_json::from_str(text);
        assert!(result.is_err());

        let text = r#""water-1""#;
        let result: Result<TokenId, _> = serde_json::from_str(text);
        assert!(result.is_err());
    }
}

//----------------------------------------------------------------------------

