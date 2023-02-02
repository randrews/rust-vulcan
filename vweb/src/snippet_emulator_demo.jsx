import { createRoot } from 'react-dom/client'
import React from 'react'
import SnippetEmulator from './snippet_emulator'
import EmulatorDisplay from './emulator_display'
import init, { WasmCPU, assemble_snippet } from '../pkg/vweb.js'

document.addEventListener('DOMContentLoaded', () => {
    init().then(() => {
        document.querySelectorAll('.snippet').forEach((el) => {
            const props = { children: el.innerText, width: el.getAttribute('data-width') }
            createRoot(el).render(React.createElement(SnippetEmulator, props))
        })
        document.querySelectorAll('.display').forEach((el) => {
            createRoot(el).render(React.createElement(EmulatorDisplay, {}))
        })
    })
})