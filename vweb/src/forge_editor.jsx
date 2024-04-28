import './snippet_emulator.css'
import React, {useCallback, useState} from 'react'

export default function ForgeEditor({ src, updateSrc }) {
    const onChangeSrc = useCallback((e) => { updateSrc(e.target.value) }, [updateSrc])
    return <textarea value={src} onChange={onChangeSrc}></textarea>
}
