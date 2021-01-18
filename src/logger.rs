//----------------------------------------------------------------------------
//! Utility functions for debug log output.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use log::{Record, Level, Metadata};
use log::{SetLoggerError, LevelFilter};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

struct SimpleLogger;

impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= Level::Trace
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            match record.level() {
                Level::Error =>
                    log(&format!("Error: {}", record.args())),
                Level::Warn =>
                    log(&format!("Warning: {}", record.args())),
                _ =>
                    log(&format!("{}", record.args()))
            }
        }
    }

    fn flush(&self) {}
}

//----------------------------------------------------------------------------

static LOGGER: SimpleLogger = SimpleLogger;

pub fn init() -> Result<(), SetLoggerError> {
    log::set_logger(&LOGGER)?;
    log::set_max_level(LevelFilter::Trace);
    Ok(())
}

//----------------------------------------------------------------------------
