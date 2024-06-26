import './snippet_emulator.css'
import React, {useEffect, useRef} from 'react'
import {basicSetup, EditorView} from 'codemirror'
import {keymap} from '@codemirror/view'
import {indentWithTab} from '@codemirror/commands'
import {StreamLanguage} from '@codemirror/language'
import {gruvboxDark} from 'cm6-theme-gruvbox-dark'
import ForgeHighlighter from './forge_highlighter'

export default function ForgeEditor({ src, updateSrc, fileName }) {
    const editor = useRef(null)
    const container = useRef(null)

    useEffect(() => {
        editor.current = new EditorView({
            doc: src,
            extensions: [
                basicSetup,
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
