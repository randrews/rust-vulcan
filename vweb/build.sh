#!/bin/bash -e

wasm-pack build --target web
npm run bundle
cp pkg/vweb_bg.wasm src/index.html src/snippet_emulator_demo.html src/basics.html src/forge_editor_demo.html src/simulator.css build
