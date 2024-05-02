import React, { useState, useCallback, useRef } from 'react'
import ForgeEditor from "./forge_editor"
import EmulatorDisplay from "./emulator_display"
import { WasmCPU, assemble_snippet, compile_forge } from '../pkg/vweb.js'
import { undent } from './undent'

export default function({ src: defaultSrc }) {
    let [activeTab, setActiveTab] = useState('editor') // Support for the tabbar
    const [assembly, setAssembly] = useState(null) // The compiled assembly code
    const [binary, setBinary] = useState(null) // The assembled binary
    const [cpu, _setCpu] = useState(() => new WasmCPU()) // The actual CPU emulator
    const [errors, setErrors] = useState(null) // What's displayed on the compile errors tab
    const [status, setStatus] = useState('') // The contents of the status bar
    // Whether the emulator should be running. Has to be a ref because the CPU setTimeout loop won't ever see changes in it otherwise
    const running = useRef(false)

    // The current Forge source. Our initial state tries to look something up from localStorage,
    // then if that fails uses the prop.
    const [src, setSrc] = useState(() => {
        return window.localStorage.getItem('src') || undent(defaultSrc)
    })

    // When we update the src, also update the key in localStorage
    const updateSrc = useCallback((newSrc) => {
        setSrc(newSrc)
        window.localStorage.setItem('src', newSrc)
    }, [])

    // Callback for the build button
    const build = useCallback(() => {
        try {
            const asm = compile_forge(src) // Compile forge to asm
            setAssembly(asm) // Store that on the asm tab for inspection
            const bin = assemble_snippet(asm) // Assemble it into a binary
            setBinary(bin) // Store that
            setStatus(`Compiled ${bin.length} bytes`) // Success!
            setErrors(null) // Clear the old error messages off the tab
        } catch (err) {
            if (err.message) { err = err.message } // How we get this differs between forge and asm
            console.error(err) // Meh
            setErrors(err) // Show full compiler errors
            setStatus("Failed to compile") // Short status line message
        }
    }, [src])

    // Callback for the run button
    const run = useCallback(() => {
        cpu.load(binary)
        cpu.reset()
        cpu.start()
        running.current = true
        setStatus('Running...')
        const time_slice = () => {
            cpu.safe_run(100_000) // We want a good number of cycles here: too short is too slow; too fast is nonresponsive
            if (!cpu.halted() && running.current) { // Stop if we've hit the stop btn or if it's at a hlt
                setTimeout(time_slice, 0) // Otherwise reschedule this for the end of the event loop
            } else {
                if (running.current) { // If this is true, we stopped from hlt, so update the status
                    // (if they hit the button then the status has already been updated)
                    setStatus('Terminated')
                    running.current = false
                }
            }
        }
        setTimeout(time_slice, 0)
    }, [cpu, binary, running])

    // Callback for the stop button
    const stop = useCallback(() => {
        running.current = false
        setStatus('Stopped by user')
    }, [])

    return (
        <>
            <Tabbar activeTab={activeTab} setActiveTab={setActiveTab} anyErrors={!!errors}/>
            <Toolbar activeTab={activeTab} running={running.current} build={build} run={run} stop={stop}/>
            <div className='content'>
                {activeTab === 'editor' && <ForgeEditor build={build} run={run} updateSrc={updateSrc} src={src}/>}
                {activeTab === 'assembly' && <pre>{assembly}</pre>}
                {activeTab === 'display' && <EmulatorDisplay cpu={cpu}/>}
                {activeTab === 'errors' && <pre>{errors}</pre>}
            </div>
            <Status message={status}/>
        </>
    )
}

// A tabbar of the various pages in the simulator
function Tabbar({ activeTab, setActiveTab, anyErrors }) {
    let onChangeTab = useCallback((event) => {
        event.preventDefault()
        setActiveTab(event.target.getAttribute('data-tab'))
    }, [setActiveTab])

    const classNames = name => (name === activeTab ? 'button active' : 'button')
    return (
        <div className='tabbar'>
            <a className={classNames('editor')} onClick={onChangeTab} data-tab='editor'>[Editor]</a>
            <a className={classNames('display')} onClick={onChangeTab} data-tab='display'>[Display]</a>
            <a className={classNames('assembly')} onClick={onChangeTab} data-tab='assembly'>[Assembly]</a>
            {anyErrors && <a className={classNames('errors')} onClick={onChangeTab} data-tab='errors'>[Errors]</a>}
        </div>
    )
}

// Toolbar of the controls for the simulator
function Toolbar({ activeTab, running, compile, build, run, stop }) {
    let buildBtn, runBtn
    // TODO: Make this assemble on assembly tab, make that editable
    buildBtn = <a className='build' onClick={build}>[Build]</a>

    if (running) {
        runBtn = <a className='stop' onClick={stop}>[Stop]</a>
    } else {
        runBtn = <a className='run' onClick={run}>[Run]</a>
    }

    return (
        <div className='buttons'>
            {buildBtn}
            {runBtn}
        </div>
    )
}

// The short status line at the bottom
function Status({ message }) {
    return <div className='status'>{message}</div>
}