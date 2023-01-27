import { createRoot } from 'react-dom/client'
import React from "react";
import SnippetEmulator from "./snippet_emulator";
import init, { WasmCPU, assemble_snippet } from '../pkg/vweb.js'

init().then(async () => {
    createRoot(document.getElementsByClassName('react-root')[0])
        .render(
            <SnippetEmulator>
                {`.org 0x400
                push 2
                add 3
                mul 4
                hlt`}
            </SnippetEmulator>
        )
})