import 'xterm/css/xterm.css'
import './app.css'
import { Terminal } from 'xterm'
import { Readline } from 'xterm-readline'
import init, { WasmCPU, NovaForth } from '../pkg/vweb.js'

const term = new Terminal()
const rl = new Readline()
const TIB = 80000 // Just a convenient place to stick a terminal input buffer for the repl. Could be any otherwise-unused address.
const OUT = 0x10000 // A place that NovaForth will use for its output buffer
let syms

term.loadAddon(rl)
term.open(document.getElementsByClassName('terminal')[0])
term.write('Loading...\r\n')

const handleKey = ({key, domEvent: { key: mnemonic }}) => {
    if (mnemonic === 'Enter') {
        console.log(mnemonic)
    } else {
        term.write(key)
    }
}

init().then(async () => {
    const cpu = new WasmCPU()
    syms = JSON.parse(NovaForth.symbols()) // Grab the symbol table for the ROM so we can poke things by name

    NovaForth.rom().forEach((byte, idx) => {
        cpu.poke(0x400 + idx, byte)
    })

    term.write(`Loaded ${NovaForth.rom().length} bytes\r\n`)
    NovaForth.prelude().split(/\n/).forEach((line) => {
        const result = eval4th(cpu, line) // If there's anything wrong with the prelude, say so
        if (result !== '') {
            console.log(`Error with "${line}": ${result}`)
        }
    })
    term.write(`Loaded NovaForth Prelude\r\n`)
    document.getElementsByClassName('prelude')[0].appendChild(document.createTextNode(NovaForth.prelude()))

    term.focus()

    while (true) {
        const line = await rl.read('> ') // Read the line of input
        const str = eval4th(cpu, line) // Run it on the CPU
        rl.println(str) // Println the result
    }
})

function eval4th(cpu, line) {
    // Copy the line to TIB and null-terminate it
    for (let i = 0; i < line.length; i++) {
        cpu.poke(TIB + i, line.charCodeAt(i))
    }
    cpu.poke(TIB + line.length, 0)

    cpu.push_call(syms.stop) // Tell the CPU to stop once we're done
    cpu.push_data(TIB) // Push the TIB and call eval
    cpu.set_pc(syms.eval)
    cpu.run() // Run it!

    const len = cpu.peek24(syms.emit_cursor) // Find the length of the output
    let str = '' // Copy the output from the memory to a JS string
    for (let i = OUT; i < OUT + len; i++) {
        str = str + String.fromCharCode(cpu.peek(i))
    }
    cpu.poke24(syms.emit_cursor, 0) // Re-clear the output buffer

    return str
}