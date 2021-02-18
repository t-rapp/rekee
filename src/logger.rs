//----------------------------------------------------------------------------
//! Utility functions for debug log output.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

#[cfg(target_arch = "wasm32")]
#[macro_export]
macro_rules! warn {
    ($($arg:tt)+) => {
        web_sys::console::warn_1(&format!($($arg)*).into());
    }
}

#[cfg(not(target_arch = "wasm32"))]
#[macro_export]
macro_rules! warn {
    ($($arg:tt)+) => {
        eprintln!("Warning: {}", &format!($($arg)*));
    }
}


#[cfg(target_arch = "wasm32")]
#[macro_export]
macro_rules! log {
    ($($arg:tt)+) => {
        web_sys::console::log_1(&format!($($arg)*).into());
    }
}

#[cfg(not(target_arch = "wasm32"))]
#[macro_export]
macro_rules! log {
    ($($arg:tt)+) => {
        eprintln!($($arg)*);
    }
}

//----------------------------------------------------------------------------
// The following log macros are active only when compiled in debug mode

#[cfg(debug_assertions)]
#[cfg(target_arch = "wasm32")]
#[macro_export]
macro_rules! info {
    ($($arg:tt)+) => {
        web_sys::console::info_1(&format!($($arg)*).into());
    }
}

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

#[cfg(debug_assertions)]
#[cfg(target_arch = "wasm32")]
#[macro_export]
macro_rules! debug {
    ($($arg:tt)+) => {
        web_sys::console::debug_1(&format!($($arg)*).into());
    }
}

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
