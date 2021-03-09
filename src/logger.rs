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
// The following log macros are active only when compiled in debug mode

/// Prints an info message to the log output.
#[cfg(debug_assertions)]
#[cfg(target_arch = "wasm32")]
#[macro_export]
macro_rules! info {
    ($($arg:tt)+) => {
        web_sys::console::info_1(&format!($($arg)*).into());
    }
}

/// Prints an info message to the log output.
#[cfg(debug_assertions)]
#[cfg(not(target_arch = "wasm32"))]
#[macro_export]
macro_rules! info {
    ($($arg:tt)+) => {
        eprintln!($($arg)*);
    }
}

#[cfg(not(debug_assertions))]
#[macro_export]
macro_rules! info {
    ($($arg:tt)+) => {
        let _ = ($($arg)*);
    };
}

/// Prints a debug message to the log output.
#[cfg(debug_assertions)]
#[cfg(target_arch = "wasm32")]
#[macro_export]
macro_rules! debug {
    ($($arg:tt)+) => {
        web_sys::console::debug_1(&format!($($arg)*).into());
    }
}

/// Prints a debug message to the log output.
#[cfg(debug_assertions)]
#[cfg(not(target_arch = "wasm32"))]
#[macro_export]
macro_rules! debug {
    ($($arg:tt)+) => {
        eprintln!($($arg)*);
    }
}

#[cfg(not(debug_assertions))]
#[macro_export]
macro_rules! debug {
    ($($arg:tt)+) => {
        let _ = ($($arg)*);
    };
}

//----------------------------------------------------------------------------
