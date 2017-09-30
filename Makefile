.PHONY: setup

setup:
	stack setup

.PHONY: build

build:
	stack build

.PHONY: dev

dev:
	stack build --fast --file-watch


.PHONY: run

run:
	stack exec imgs

.PHONY: clean_all

clean_all:
	stack clean

