//----------------------------------------------------------------------------
//! Build script that generates CSS from SASS.
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.
//----------------------------------------------------------------------------

use std::env;
use std::fs;
use std::path::Path;

use grass::{self, OutputStyle};

fn write_bulma_style(src_dir: &str, dest_dir: &str) {
    let src_file = Path::new(src_dir).join("bulma.scss");
    let dest_file = Path::new(dest_dir).join("bulma.min.css");
    let options = grass::Options::default()
        .style(OutputStyle::Compressed);
    let css = grass::from_path(src_file, &options)
        .unwrap();
    fs::write(dest_file, &css).unwrap();
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={}", dest_dir);
}

fn main() {
    let style_enabled = env::var_os("CARGO_FEATURE_STYLE").is_some();
    if style_enabled {
        write_bulma_style("style", "www");
    }
}
