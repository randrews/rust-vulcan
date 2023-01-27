import './snippet_emulator.css'
import React, {useState, useCallback, useEffect} from 'react'
import {WasmCPU, assemble_snippet, source_map, NovaForth} from '../pkg/vweb.js'

export default function({ children }) {
    if (React.Children.count(children) !== 1) {
        throw 'Expects a single text node as a child'
    }

    const [editing, setEditing] = useState(false) // Whether we're editing the snippet or running it
    const [src, setSrc] = useState('') // The source code currently set
    const [message, setMessage] = useState('') // A status / error message
    const onChangeSrc = useCallback((e) => setSrc(e.target.value), []) // Callback for the textarea
    const [binary, setBinary] = useState(null) // The assembled binary
    const [sourceMap, setSourceMap] = useState(null) // A map from byte address to line number
    const [cpu, setCpu] = useState(new WasmCPU()) // The actual CPU emulator itself
    const [activeLine, setActiveLine] = useState(null) // The (0-based) index of the

    const updateActiveLine = useCallback(() => {
        sourceMap && setActiveLine(sourceMap.get(cpu.pc()))
    }, [cpu, sourceMap])

    const reset = useCallback(() => {
        cpu.reset()
        cpu.start()
        updateActiveLine()
    }, [cpu, updateActiveLine])

    // A callback to build the binary from source
    const rebuild = useCallback((code) => {
        try {
            const bin = assemble_snippet(code)
            const sm = source_map(code)
            setBinary(bin)
            setSourceMap(sm.Ok || {})
            setMessage(`Assembled ${bin.length} bytes`)
            return true
        } catch(e) {
            setMessage(e.message)
        }
    }, [])

    const step = useCallback(() => {
        cpu.tick()
        updateActiveLine()
    }, [cpu, updateActiveLine])

    // On load, clean the source and build the stuff
    useEffect(() => {
        const lines = React.Children.toArray(children)[0].split('\n')
        const cleaned = lines.map(l => l.trim()).join('\n')
        setSrc(cleaned)
        rebuild(cleaned)
    }, [children])

    // Load and reset the CPU when the assembled binary changes
    useEffect(() => {
        if (binary) {
            cpu.load(binary)
            reset()
        }
    }, [cpu, binary, reset])

    if (editing) {
        return (
            <div className='snippetEmulator'>
                <textarea value={src} onChange={onChangeSrc}></textarea>
                <div className='message'>{message}</div>
                <div className='buttons'>
                    <a className='build' onClick={() => { rebuild(src) && setEditing(false) }}>[Build]</a>
                </div>
            </div>
        )
    } else {
        return (
            <div className='snippetEmulator'>
                <SourceDisplay src={src} activeLine={activeLine}/>
                <div className='message'>{message}</div>
                <div className='buttons'>
                    <a className='step' onClick={step}>[Step]</a>
                    <a className='step'>[Run]</a>
                    <a className='reset' onClick={reset}>[Reset]</a>
                    <a className='edit' onClick={() => { setEditing(true); setMessage('') }}>[Edit]</a>
                </div>
            </div>
        )
    }
}

function SourceDisplay({ src, activeLine }) {
    const lines = src.split('\n')
    const lineDivs = lines.map((line, i) => {
        // Loop indices are from 0, activeLine numbers are from 1
        return (<div key={`line_${i}`} className={i + 1 === activeLine ? 'highlight' : ''}>{line || <>&nbsp;</>}</div>)
    })
    return (
        <div className='src'>{lineDivs}</div>
    )
}

function StackDisplay({ data, call }) {

}