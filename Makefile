MAKEFLAGS += --no-print-directory --always-make

# all: Default target, install binary
all: install

# help: Print help
help: Makefile
	@printf "\033[1mUsage: make <TARGETS> ...\033[0m\n\n\033[1mTargets:\033[0m\n"
	@sed -n 's/^# //p' $< | awk -F':' '{printf "\033[36m%-12s\033[0m %s\n", $$1, $$2}' | sort | sed -e 's/^/  /'

# install: Install the CLI
install:
	@cargo install --path ./coal

# build: Build binary
build:
	@cargo build --release

# clean: Clean build artifacts
clean:
	@cargo clean

# run: Run the CLI
run:
	@cargo run

# lint: Run clippy
lint:
	@cargo clippy

# test: Run tests
test:
	@cargo test

# docs: Generate docs
docs:
	@cargo doc --no-deps --document-private-items --open

# vsc: Install the VSCode language extension
vsc:
	@(cd ./extensions/vscode-coal && make)
