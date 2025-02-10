version := $(shell node scripts/version.js)

.PHONY: all install version

all: install

install:
	@npm run package
	@code --install-extension coal-$(version).vsix
	@rm coal-$(version).vsix

version:
	@echo $(version)
