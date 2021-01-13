Rekee
=====

Rekee is an editor that allows designing of custom tracks for the boardgames
[Rallyman: GT] and [Rallyman: DIRT] published by Holy Grail Games. This software
is a personal project with the goal to build a cross-platform web application as
an alternative to the currently existing [Rallyman Track Editor][track editor],
which is Windows-only.

[Rallyman: GT]: https://holygrail.games/en/games/rallyman-gt/
[Rallyman: DIRT]: https://holygrail.games/en/games/rallyman-dirt/
[track editor]: https://boardgamegeek.com/thread/2399829/upload-new-track-editor-v15-9th-may-2020

## Development

The editor is implemented as a WebAssembly module, loaded into the main HTML
page using a tiny JavaScript code for bootstrap.

Building requires installation of:
 * [Rust](https://www.rust-lang.org/tools/install)
   with the `wasm32-unknown-unknown` target
 * [Wasm-Pack](https://rustwasm.github.io/wasm-pack/installer/)

Command for building the WebAssembly module:
```
wasm-pack build --target web --release --out-dir www/pkg
```

(Replace the `--release` flag with `--dev` to include more verbose debug
information within the generated module).

Command for running a local instance of the web application:
```
(cd www/ && python3 -m http.server 8000)
```

## License

Source code for Rekee is distributed under the terms of the Mozilla Public
License 2.0 (MPL-2.0). See [LICENSE](LICENSE.txt) for details.

Copyright owner for boardgame tile images is [Holy Grail Games][HGG], who gave
permission to use the images within Rekee. Thanks a lot!

This project uses Bootstrap Icons distributed under the terms of the MIT License,
see [LICENSE-BootstrapIcons](LICENSE-BootstrapIcons.txt) for details.

This project uses Bulma CSS framework distributed under the terms of the MIT
License, see [LICENSE-Bulma](LICENSE-Bulma.txt) for details.

[HGG]: https://holygrail.games/en/
