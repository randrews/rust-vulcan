import './snippet_emulator.css'
import React, {useCallback, useState, useRef, useEffect} from 'react'
import {basicSetup, EditorView} from 'codemirror'
import {gruvboxDark} from 'cm6-theme-gruvbox-dark'

export default function ForgeEditor({ src, updateSrc }) {
    const editor = useRef(null)
    const container = useRef(null)

    useEffect(() => {
        if (container.current) {
            editor.current = new EditorView({
                doc: src,
                extensions: [
                    basicSetup,
                    gruvboxDark,
                    EditorView.updateListener.of((update) => {
                        updateSrc(update.state.doc.text.join('\n'))
                    })
                ],
                parent: container.current
            })
        }
    }, [container.current])

    const onChangeSrc = useCallback((e) => { updateSrc(e.target.value) }, [updateSrc])
    return <div className='editor' ref={container}/>
}
