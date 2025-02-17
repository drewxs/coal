version := $(shell node scripts/version.js)

.PHONY: all install version

all: install

install:
	@./scripts/install.sh

version:
	@echo $(version)
