import './snippet_emulator.css'
import React, {useEffect, useRef} from 'react'
import {basicSetup, EditorView} from 'codemirror'
import {keymap} from '@codemirror/view'
import {indentWithTab} from '@codemirror/commands'
import {StreamLanguage} from '@codemirror/language'
import {gruvboxDark} from 'cm6-theme-gruvbox-dark'
import ForgeHighlighter from './forge_highlighter'
import { Prec } from "@codemirror/state"

export default function ForgeEditor({ src, updateSrc, fileName }) {
    const editor = useRef(null)
    const container = useRef(null)

    useEffect(() => {
        editor.current = new EditorView({
            doc: src,
            extensions: [
                basicSetup,
                // Normally F3 opens the search dialog, but we want to just eat it so the document's handler will get it.
                // We use F3 as a global shortcut for resetting the machine, even if the editor has focus.
                // Since the editor doesn't do anything for F1, 2, or 4, we don't need to do anything to ignore those.
                Prec.highest(
                    keymap.of([{ key: "F3", run: () => { return true } }])
                ),
                keymap.of([indentWithTab]),
                gruvboxDark,
                EditorView.updateListener.of((update) => {
                    updateSrc(update.state.doc.toString())
                }),
                StreamLanguage.define(ForgeHighlighter)
            ],
            parent: container.current
        })

        // Destroy the editor when we re-fire this, which is when the filename changes
        return () => (container.current && container.current.children[0].remove())
    }, [fileName])

    return <div className='editor' ref={container}/>
}
