.PHONY: all version install

all: install

version:
	@grep '"version"' package.json | sed -E 's/.*"version": "(.*)",/\1/'

install:
	@./scripts/install.sh

