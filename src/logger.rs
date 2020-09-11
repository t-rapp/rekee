//----------------------------------------------------------------------------
// File: logger.rs
// $Id$
//----------------------------------------------------------------------------

use log::{Record, Level, Metadata};
use log::{SetLoggerError, LevelFilter};

struct SimpleLogger;

impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= Level::Trace
    }

    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            match record.level() {
                Level::Error =>
                    eprintln!("Error: {}", record.args()),
                Level::Warn =>
                    eprintln!("Warning: {}", record.args()),
                _ =>
                    eprintln!("{}", record.args())
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
