import React, {useState, useCallback, useRef, useEffect} from 'react'
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

    const [currentFile, setCurrentFile] = useState(() => (Object.keys(window.localStorage).sort()[0] || 'example'))
    useEffect(() => {
        if (!currentFile) {
            setCurrentFile('example')
            window.localStorage.setItem('example', defaultSrc)
        }
    }, [])

    const selectFile = useCallback((name) => {
        setSrc(window.localStorage.getItem(name))
        setCurrentFile(name)
    }, [])

    const addFile = useCallback(() => {
        const fileList = Object.keys(window.localStorage)
        let name = null
        while(!name || fileList.indexOf(name) >= 0) {
            name = prompt('Create new file named:')
        }
        window.localStorage[name] = ''
        selectFile(name)
    }, [])

    const removeFile = useCallback(() => {
        const files = Object.keys(window.localStorage).sort()
        if (files.length < 2) { return } // Can't delete the last file
        if (!confirm(`Are you sure you want to delete "${currentFile}"?`)) { return }
        const idx = files.indexOf(currentFile)
        window.localStorage.removeItem(currentFile)
        files.splice(idx, 1)
        selectFile(files[Math.min(files.length - 1, idx)])
    }, [currentFile, selectFile])

    // The current Forge source. Our initial state tries to look something up from localStorage,
    // then if that fails uses the prop.
    const [src, setSrc] = useState(() => {
        return window.localStorage.getItem(currentFile) || undent(defaultSrc)
    })

    // When we update the src, also update the key in localStorage
    const updateSrc = useCallback((newSrc) => {
        setSrc(newSrc)
        window.localStorage.setItem(currentFile, newSrc)
    }, [currentFile])

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

    let content;
    if (activeTab === 'assembly') {
        content = <pre>{assembly}</pre>
    } else if (activeTab === 'errors') {
        content = <pre>{errors}</pre>
    } else if (activeTab === 'editor') {
        content = <ForgeEditor build={build} run={run} updateSrc={updateSrc} src={src} fileName={currentFile}/>
    }

    return (
        <>
            <Tabbar activeTab={activeTab} setActiveTab={setActiveTab} anyErrors={!!errors}/>
            <Toolbar activeTab={activeTab} running={running.current} build={build} run={run} stop={stop} addFile={addFile} removeFile={removeFile}/>
            <FileList fileList={Object.keys(window.localStorage).sort()} selectFile={selectFile} currentFile={currentFile}/>
            <div className='content'>{content}</div>
            <EmulatorDisplay cpu={cpu}/>
            <Status message={status}/>
        </>
    )
}

// A list of "files" stored in localStorage, that we can display / build / compile
function FileList({ fileList, selectFile, currentFile }) {
    const clickFile = useCallback((event) => {
        const name = event.target.getAttribute('data-name')
        selectFile(name)
        event.preventDefault()
    }, [selectFile])
    const fileRows = fileList.map(file => <div className={currentFile === file ? 'selected-file file' : 'file'} onClick={clickFile} data-name={file} key={file}>{file}</div>)
    return (
        <div className='files'>
            {fileRows}
        </div>
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
            <a className={classNames('assembly')} onClick={onChangeTab} data-tab='assembly'>[Assembly]</a>
            {anyErrors && <a className={classNames('errors')} onClick={onChangeTab} data-tab='errors'>[Errors]</a>}
        </div>
    )
}

// Toolbar of the controls for the simulator
function Toolbar({activeTab, running, compile, build, run, stop, addFile, removeFile }) {
    let buildBtn, runBtn
    // TODO: Make this assemble on assembly tab, make that editable
    buildBtn = <a className='build' onClick={build}>[Build]</a>

    if (running) {
        runBtn = <a className='stop' onClick={stop}>[Stop]</a>
    } else {
        runBtn = <a className='run' onClick={run}>[Run]</a>
    }

    return (
        <>
            <div className='file-buttons'>
                <a className='new' onClick={addFile}>[new]</a>
                <a className='del' onClick={removeFile}>[del]</a>
            </div>
            <div className='buttons'>
                {buildBtn}
                {runBtn}
            </div>
        </>
    )
}

// The short status line at the bottom
function Status({message}) {
    return <div className='status'>{message}</div>
}