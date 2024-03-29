Rekee
=====

Rekee is an editor that allows to design custom tracks for the [Rallyman: GT]
and [Rallyman: DIRT] board games published by Holy Grail Games. This software
project has the goal to build a cross-platform web application as an
alternative to the currently existing [Rallyman Track Editor][track editor],
which is Windows-only.

[Rallyman: GT]: https://boardgamegeek.com/boardgame/256589/rallyman-gt
[Rallyman: DIRT]: https://boardgamegeek.com/boardgame/312959/rallyman-dirt
[track editor]: https://boardgamegeek.com/thread/2399829/upload-new-track-editor-v15-9th-may-2020

## Development

The editor is implemented as a WebAssembly module, loaded into the main HTML
page using a tiny JavaScript code for bootstrap.

Building requires installation of:
 * [Rust](https://www.rust-lang.org/tools/install)
   with the `wasm32-unknown-unknown` target
 * [Wasm-Pack](https://rustwasm.github.io/wasm-pack/installer/)

Command for building the WebAssembly module and CSS file:
```
make build
```

(Use `make build-release` to create an optimzed WASM module with stripped debug
symbols).

Command for running a local instance of the web application:
```
make run
```

### Examples

The non-visual parts of Rekee can also be used as a library outside the
WebAssembly environment, they allow to open and process track data files. See
the files in the `examples` subfolder for some simple stand-alone applications.

### Source Documentation

To generate HTML code documentation for the library sources use command:
```
make doc
```
Then open the file `target/doc/rekee/index.html` in your web browser.

Alternatively you can find auto-generated source documentation from the main
branch on GitHub at <https://t-rapp.github.io/rekee/rekee/index.html>.

### Unit Tests

The source files contain a set of unit tests. Before committing any code
changes it is recommended to run these tests with:
```
make test
```

(Note that this will compile and run test code using the default native Rust
target, which is not exactly the same as running the WASM target output inside
its virtual environment. It should make no practical difference, though.)

## License

Source code for Rekee is distributed under the terms of the Mozilla Public
License 2.0 (MPL-2.0). See [LICENSE](LICENSE.txt) for details.

Copyright owner for boardgame tile images is/was [Holy Grail Games][HGG], who
gave permission to use the images within Rekee. Thanks a lot!

This project uses Bootstrap Icons distributed under the terms of the MIT License,
see [LICENSE-BootstrapIcons](LICENSE-BootstrapIcons.txt) for details.

This project uses Bulma CSS framework distributed under the terms of the MIT
License, see [LICENSE-Bulma](LICENSE-Bulma.txt) for details.

[HGG]: https://web.archive.org/web/20230216122339/https://holygrail.games/
