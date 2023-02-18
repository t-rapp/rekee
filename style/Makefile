# ---------------------------------------------------------------------------
# Makefile for building the Bulma CSS file
# ---------------------------------------------------------------------------

BULMA_VERSION := 0.9.4
GRASS_VERSION := $(shell grass --version 2>/dev/null)

.PHONY: all
all: fetch

.PHONY: build
build: bulma bulma.scss
ifndef GRASS_VERSION
	$(error 'grass is not available, you can install it by running "cargo install grass"')
endif
	grass --style compressed bulma.scss bulma.min.css
	mv -f bulma.min.css ../www/bulma.min.css

.PHONY: clean
clean:
	rm -rf bulma

.PHONY: fetch
fetch: bulma

bulma: bulma-$(BULMA_VERSION).zip
	unzip -o $^
	touch $@

bulma-$(BULMA_VERSION).zip:
	curl --output $@ --location https://github.com/jgthms/bulma/releases/download/$(BULMA_VERSION)/bulma-$(BULMA_VERSION).zip
