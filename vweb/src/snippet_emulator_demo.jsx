import { createRoot } from 'react-dom/client'
import React from 'react'
import SnippetEmulator from './snippet_emulator'
import init, { WasmCPU, assemble_snippet } from '../pkg/vweb.js'

document.addEventListener('DOMContentLoaded', () => {
    init().then(() => {
        document.querySelectorAll('.snippet').forEach((el) => {
            createRoot(el).render(React.createElement(SnippetEmulator, { children: el.innerText }))
        })
    })
})