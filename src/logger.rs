//----------------------------------------------------------------------------
//! Utility functions for debug log output.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

/// Prints a warning message to the log output.
#[cfg(target_arch = "wasm32")]
#[macro_export]
macro_rules! warn {
    ($($arg:tt)+) => {
        web_sys::console::warn_1(&format!($($arg)*).into());
    }
}

/// Prints a warning message to the log output.
#[cfg(not(target_arch = "wasm32"))]
#[macro_export]
macro_rules! warn {
    ($($arg:tt)+) => {
        eprintln!("Warning: {}", &format!($($arg)*));
    }
}

/// Prints a generic message to the log output.
#[cfg(target_arch = "wasm32")]
#[macro_export]
macro_rules! log {
    ($($arg:tt)+) => {
        web_sys::console::log_1(&format!($($arg)*).into());
    }
}

/// Prints a generic message to the log output.
#[cfg(not(target_arch = "wasm32"))]
#[macro_export]
macro_rules! log {
    ($($arg:tt)+) => {
        eprintln!($($arg)*);
    }
}

//----------------------------------------------------------------------------
// The following log macros are active only when compiled with the "logger" feature

/// Prints an info message to the log output.
///
/// Log output is skipped when not compiled with the "logger" feature.
#[cfg(target_arch = "wasm32")]
#[macro_export]
macro_rules! info {
    ($($arg:tt)+) => {
        if cfg!(feature = "logger") {
            web_sys::console::info_1(&format!($($arg)*).into());
        }
    }
}

/// Prints an info message to the log output.
///
/// Log output is skipped when not compiled with the "logger" feature.
#[cfg(not(target_arch = "wasm32"))]
#[macro_export]
macro_rules! info {
    ($($arg:tt)+) => {
        if cfg!(feature = "logger") {
            eprintln!($($arg)*);
        }
    }
}

/// Prints a debug message to the log output.
///
/// Log output is skipped when not compiled with the "logger" feature.
#[cfg(target_arch = "wasm32")]
#[macro_export]
macro_rules! debug {
    ($($arg:tt)+) => {
        if cfg!(feature = "logger") {
            web_sys::console::debug_1(&format!($($arg)*).into());
        }
    }
}

/// Prints a debug message to the log output.
///
/// Log output is skipped when not compiled with the "logger" feature.
#[cfg(not(target_arch = "wasm32"))]
#[macro_export]
macro_rules! debug {
    ($($arg:tt)+) => {
        if cfg!(feature = "logger") {
            eprintln!($($arg)*);
        }
    }
}

//----------------------------------------------------------------------------
