import { createRoot } from 'react-dom/client'
import React from "react";
import SnippetEmulator from "./snippet_emulator";
import init, { WasmCPU, assemble_snippet } from '../pkg/vweb.js'

init().then(async () => {
    createRoot(document.getElementsByClassName('react-root')[0])
        .render(React.createElement(SnippetEmulator, {snippet: 'push 0'}))
})