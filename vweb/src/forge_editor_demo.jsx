import { createRoot } from 'react-dom/client'
import React from 'react'
import ForgeSimulator from './forge_simulator'
import init, { WasmCPU, assemble_snippet } from '../pkg/vweb.js'

document.addEventListener('DOMContentLoaded', () => {
    init().then(() => {
        const root = document.querySelector('.simulator')
        createRoot(root).render(React.createElement(ForgeSimulator, { src: root.innerText }))
    })
})