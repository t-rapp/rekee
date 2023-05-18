# ---------------------------------------------------------------------------
# Makefile for building the Rekee WASM module
# ---------------------------------------------------------------------------

WASMOPT_VERSION := $(shell wasm-opt --version 2>/dev/null)

.PHONY: all
all: build doc

.PHONY: build
build:
	$(MAKE) -C style fetch
	wasm-pack build --target web --dev --out-dir www/pkg --no-typescript --features logger,style
	@# Fix the missing trailing newline
	@echo "" >> www/pkg/.gitignore

.PHONY: build-release
build-release:
	$(MAKE) -C style fetch
	wasm-pack build --target web --release --out-dir www/pkg --no-typescript --features style
	@# Fix the missing trailing newline
	@echo "" >> www/pkg/.gitignore
ifdef WASMOPT_VERSION
	# Second WASM optimization run
	wasm-opt www/pkg/rekee_bg.wasm -o www/pkg/rekee_bg.wasm.tmp -O -Oz
	mv www/pkg/rekee_bg.wasm.tmp www/pkg/rekee_bg.wasm
endif
	# Binary size of the generated WASM module
	@ls --size --human-readable -1 www/pkg/*.wasm

.PHONY: check
check: build test

.PHONY: clean
clean:
	$(MAKE) -C style clean
	cargo clean

.PHONY: doc
doc:
	cargo doc --lib --no-deps

.PHONY: examples
examples:
	cargo build --release --examples --features svg

.PHONY: run
run: build style
	cd www && python3 -m http.server 8080

.PHONY: test
test:
	cargo test
	cargo +stable clippy --all-targets --all-features

