import './snippet_emulator.css'
import React, {useState, useCallback, useEffect} from 'react'
import { WasmCPU, assemble_snippet, source_map } from '../pkg/vweb.js'

export default function({ children }) {
    if (React.Children.count(children) !== 1) {
        throw 'Expects a single text node as a child'
    }

    const [editing, setEditing] = useState(false) // Whether we're editing the snippet or running it
    const [src, setSrc] = useState('') // The source code currently set
    const [message, setMessage] = useState('') // A status / error message
    const onChangeSrc = useCallback((e) => setSrc(e.target.value), []) // Callback for the textarea
    const [binary, setBinary] = useState(null) // The assembled binary
    const [sourceMap, setSourceMap] = useState({}) // A map from byte address to line number

    // A callback to build the binary from source
    const rebuild = useCallback((code) => {
        try {
            const bin = assemble_snippet(code)
            const sm = source_map(code)
            setBinary(bin)
            setSourceMap(sm)
            setMessage(`Assembled ${bin.length} bytes`)
            return true
        } catch(e) {
            setMessage(e.message)
        }
    }, [])

    // On load, clean the source and build the stuff
    useEffect(() => {
        const lines = React.Children.toArray(children)[0].split('\n')
        const cleaned = lines.map(l => l.trim()).join('\n')
        setSrc(cleaned)
        rebuild(cleaned)
    }, [rebuild, children])

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
                <SourceDisplay src={src}/>
                <div className='message'>{message}</div>
                <div className='buttons'>
                    <a className='step'>[Step]</a>
                    <a className='step'>[Run]</a>
                    <a className='reset'>[Reset]</a>
                    <a className='edit' onClick={() => { setEditing(true); setMessage('') }}>[Edit]</a>
                </div>
            </div>
        )
    }
}

function SourceDisplay({ src }) {
    const lines = src.split('\n')
    const lineDivs = lines.map((line, i) => {
        return (<div key={`line_${i}`}>{line || <>&nbsp;</>}</div>)
    })
    return (
        <div className='src'>{lineDivs}</div>
    )
}

function StackDisplay({ data, call }) {

}