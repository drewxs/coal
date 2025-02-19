program := coal

debug ?=
ifdef debug
  release :=
  target := debug
  extension := debug
else
  release := --release
  target := release
  extension :=
endif

.PHONY: help build clean run test install vsc

# all: Default target, build/install binary
all: install

# help: Print help
help: Makefile
	@printf "\033[1mUsage: make <TARGETS> ...\033[0m\n\n\033[1mTargets:\033[0m\n"
	@sed -n 's/^# //p' $< | awk -F':' '{printf "\033[36m%-12s\033[0m %s\n", $$1, $$2}' | sort | sed -e 's/^/  /'

# build: Build binary
build:
	@cargo build $(release)

# clean: Clean build artifacts
clean:
	@cargo clean

# run: Run the CLI
run:
	@cargo run

# test: Run tests
test:
	@cargo test

# install: Install the program
install: 
	@cargo install --path ./$(program)

# vsc: Install the VSCode language extension
vsc:
	@(cd ./extensions/vscode-coal && make)
