import './snippet_emulator.css'
import React, { useState, useCallback, useEffect } from 'react'
import { WasmCPU, assemble_snippet, source_map } from '../pkg/vweb.js'
import HighlightedLine, { indent } from './highlighted_line'

export default function({ children, width = 20 }) {
    // Use this by giving it some source as a body:
    // <SnippetEmulator>
    //   {`.org 0x400
    //     push 3
    //     hlt`}
    // </SnippetEmulator>
    // The string-in-braces is so that jsx doesn't eat the newlines and whitespace
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

    // Update which line is highlighted. This should be called in most button callbacks.
    const updateActiveLine = useCallback(() => {
        sourceMap && setActiveLine(sourceMap.get(cpu.pc()))
        if (cpu.halted()) { setMessage('Halted') }
    }, [cpu, sourceMap])

    // Reset the CPU
    const reset = useCallback(() => {
        cpu.reset()
        cpu.start() // Reset leaves Vulcan in a stopped state but since we're going to step through anyway...
        updateActiveLine()
    }, [cpu, updateActiveLine])

    // Reset the CPU but also put in a message saying we did. This needs to be a separate fn because the
    // rebuild handler calls the other one but it sets its own message which we don't want to overwrite
    const resetBtn = useCallback(() => {
        reset()
        setMessage('Reset')
    }, [reset])

    // A callback to build the binary from source
    const rebuild = useCallback((code) => {
        try {
            const bin = assemble_snippet(code) // Build the thing and its source map. This throws if there's a problem
            const sm = source_map(code)
            setBinary(bin)
            setSourceMap(sm.Ok || {})
            setMessage(`Assembled ${bin.length} bytes`)
            return true
        } catch(e) {
            setMessage(e.message)
        }
    }, [])

    // Step forward one
    const step = useCallback(() => {
        cpu.tick()
        setMessage('')
        updateActiveLine()
    }, [cpu, updateActiveLine])

    // Run all the lines at once, until the CPU halts
    const runToHalt = useCallback(() => {
        setTimeout(() => {
            cpu.safe_run(100_000) // Safe run, 100k cycles and pause
            updateActiveLine()
            // If we're not halted then we're probably in an infinite loop. That might be fine, but stop running, in case
            // it's not. If they just hit [run] again then it'll keep on going. For snippets it's likely this indicates
            // a bug though.
            if (!cpu.halted()) { setMessage('Paused after 100k cycles')}
        }, 0)
        setMessage('Running...')
    }, [cpu, updateActiveLine])

    // On load, clean the source and build the stuff
    useEffect(() => {
        const lines = React.Children.toArray(children)[0].split('\n')
        const cleaned = lines.map(indent).join('\n')
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
            <div className='snippetEmulator' style={{ gridTemplateColumns: `${width}em` }}>
                <textarea value={src} onChange={onChangeSrc}></textarea>
                <div className='message'>{message}</div>
                <div className='buttons'>
                    <a className='build' onClick={() => { rebuild(src) && setEditing(false) }}>[Build]</a>
                </div>
            </div>
        )
    } else {
        return (
            <div className='snippetEmulator' style={{ gridTemplateColumns: `${width}em` }}>
                <SourceDisplay src={src} activeLine={activeLine}/>
                <div className='message'>{message}</div>
                <div className='buttons'>
                    <a className='step' onClick={step}>[Step]</a>
                    <a className='step' onClick={runToHalt}>[Run]</a>
                    <a className='reset' onClick={resetBtn}>[Reset]</a>
                    <a className='edit' onClick={() => { setEditing(true); setMessage('') }}>[Edit]</a>
                </div>
                <StackDisplay data={cpu.get_stack()} call={cpu.get_call()}/>
            </div>
        )
    }
}

function SourceDisplay({ src, activeLine }) {
    const lines = src.split('\n')
    const lineDivs = lines.map((line, i) => {
        // Loop indices are from 0, activeLine numbers are from 1
        return (
            <div key={`line_${i}`} className={i + 1 === activeLine ? 'highlight' : ''}>
                {<HighlightedLine line={line}/> || <>&nbsp;</>}
            </div>
        )
    })
    return (
        <div className='src'>{lineDivs}</div>
    )
}

function StackDisplay({ data, call }) {
    return (
        <div className='stacks'>
            {data && <div><span>Data:&nbsp;</span><span>{data.join(', ')}</span></div>}
            {call && <div><span>Call:&nbsp;</span><span>{call.join(', ')}</span></div>}
        </div>
    )
}