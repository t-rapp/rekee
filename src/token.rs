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

//----------------------------------------------------------------------------

/// Identifier for a token which can be placed on tiles.
///
/// # Examples
///
/// ```
/// # use rekee::token::*;
/// let token: TokenId = "chicane".parse().unwrap();
/// assert_eq!(token, TokenId::Chicane);
///
/// let token = TokenId::Jump;
/// assert_eq!(token.to_string(), "Jump");
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[derive(Deserialize)]
#[serde(try_from = "&str")]
pub enum TokenId {
    /// Chicane token without a speed limit, part of the Rallyman DIRT Copilot Pack.
    Chicane,
    /// Chicane token with a speed limit, part of the Rallyman DIRT Copilot Pack.
    ChicaneWithLimit,
    /// Jump token, part of the Rallyman DIRT Copilot Pack.
    Jump,
    /// Water token, part of the Rallyman DIRT Copilot Pack.
    Water,
    /// Joker entrance arch, part of the RX expansion for Rallyman DIRT.
    JokerEntrance,
    /// Joker exit arch, part of the RX expansion for Rallyman DIRT.
    JokerExit,
    /// Finishing line arch, part of the RX expansion for Rallyman DIRT.
    Finish,
}

impl fmt::Display for TokenId {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenId::Chicane =>
                write!(fmt, "Chicane"),
            TokenId::ChicaneWithLimit =>
                write!(fmt, "Chicane-Limit"),
            TokenId::Jump =>
                write!(fmt, "Jump"),
            TokenId::Water =>
                write!(fmt, "Water"),
            TokenId::JokerEntrance =>
                write!(fmt, "Joker-Entrance"),
            TokenId::JokerExit =>
                write!(fmt, "Joker-Exit"),
            TokenId::Finish =>
                write!(fmt, "Finish")
        }
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

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let name = s;
        if name.eq_ignore_ascii_case("chicane") {
            Ok(TokenId::Chicane)
        } else if name.eq_ignore_ascii_case("chicane-limit") {
            Ok(TokenId::ChicaneWithLimit)
        } else if name.eq_ignore_ascii_case("jump") {
            Ok(TokenId::Jump)
        } else if name.eq_ignore_ascii_case("water") {
            Ok(TokenId::Water)
        } else if name.eq_ignore_ascii_case("joker-entrance") {
            Ok(TokenId::JokerEntrance)
        } else if name.eq_ignore_ascii_case("joker-exit") {
            Ok(TokenId::JokerExit)
        } else if name.eq_ignore_ascii_case("finish") {
            Ok(TokenId::Finish)
        } else {
            Err(ParseTokenError::UnknownName(name.to_string()))
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
        let mut data = self.to_string();
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
        assert_eq!(TokenId::Chicane.to_string(), "Chicane");
        assert_eq!(TokenId::ChicaneWithLimit.to_string(), "Chicane-Limit");
        assert_eq!(TokenId::Jump.to_string(), "Jump");
        assert_eq!(TokenId::Water.to_string(), "Water");
        assert_eq!(TokenId::JokerEntrance.to_string(), "Joker-Entrance");
        assert_eq!(TokenId::JokerExit.to_string(), "Joker-Exit");
        assert_eq!(TokenId::Finish.to_string(), "Finish");

        let text = format!("{:x}", TokenId::Chicane);
        assert_eq!(text, "chicane");
        let text = format!("{:x}", TokenId::Jump);
        assert_eq!(text, "jump");
        let text = format!("{:x}", TokenId::JokerExit);
        assert_eq!(text, "joker-exit");
    }

    #[test]
    fn token_from_str() {
        assert_eq!("chicane".parse::<TokenId>(), Ok(TokenId::Chicane));
        assert_eq!("chicane-limit".parse::<TokenId>(), Ok(TokenId::ChicaneWithLimit));
        assert_eq!("Jump".parse::<TokenId>(), Ok(TokenId::Jump));
        assert_eq!("WATER".parse::<TokenId>(), Ok(TokenId::Water));
        assert_eq!("Joker-Entrance".parse::<TokenId>(), Ok(TokenId::JokerEntrance));
        assert_eq!("jOKER-eXIT".parse::<TokenId>(), Ok(TokenId::JokerExit));
        assert_eq!("FiNiSh".parse::<TokenId>(), Ok(TokenId::Finish));

        assert!("".parse::<TokenId>().is_err());
        assert!("jump-4".parse::<TokenId>().is_err());
        assert!("joker".parse::<TokenId>().is_err());
    }

    #[test]
    fn token_serde() {
        let token = TokenId::Chicane;
        let text = serde_json::to_string(&token).unwrap();
        assert_eq!(text, r#""chicane""#);

        let token = TokenId::ChicaneWithLimit;
        let text = serde_json::to_string(&token).unwrap();
        assert_eq!(text, r#""chicane-limit""#);

        let token = TokenId::Jump;
        let text = serde_json::to_string(&token).unwrap();
        assert_eq!(text, r#""jump""#);

        let text = r#""water""#;
        let token: TokenId = serde_json::from_str(text).unwrap();
        assert_eq!(token, TokenId::Water);

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

