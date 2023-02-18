Rekee Style
===========

Rekee uses the [Bulma](https://bulma.io/) CSS framework. This folder contains
the settings that are used to generate the file in `www/bulma.min.css`.

The CSS output file is generated as part of the build process triggered by
running `make` in the parent folder. If you just want to create the CSS
without running a full Cargo build the [`grass`](https://crates.io/crates/grass)
command-line tool needs to be available.

To install `grass` on your system use:
```
cargo install grass
```

After changes to `bulma.scss`, generate the CSS output file with:
```
make build
```

See <https://bulma.io/documentation/customize/concepts/> for more information.
