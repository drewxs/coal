#!/bin/bash

VERSION=$(grep '"version"' package.json | sed -E 's/.*"version": "(.*)",/\1/')
EXT_BIN="coal-$VERSION.vsix"

if [[ -x "$(command -v 'node')" ]]; then
    npm install
    npm run package

    code --install-extension "$EXT_BIN"

    rm "$EXT_BIN"
elif [[ -x "$(command -v 'docker')" ]]; then
    mkdir -p output

    docker build -t vscode-coal .
    docker run -v "$(pwd)/output:/output" vscode-coal cp -r "/app/$EXT_BIN" "/output/$EXT_BIN"

    code --install-extension "./output/$EXT_BIN"

    rm -rf output
else
    echo "Error: Node.js or Docker is required to build the extension."
fi
