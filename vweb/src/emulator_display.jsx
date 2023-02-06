import React, { useCallback, useEffect, useRef } from 'react'
import { WasmCPU, Display } from '../pkg/vweb.js'

export default function({}) {
    const canvas = useRef(null)
    const cpu = useRef(new WasmCPU())
    const ctx = useRef(null)
    const display = useRef(null)
    const request = useRef(null)

    const blah = useRef(0)

    const drawFrame = useCallback(() => {
        // for(let n = 0; n < 160 * 120; n++) {
        //     cpu.current.poke(0x10000 + n, n % 256 /* Math.random() * 256 */)
        // }
        display.current.draw(cpu.current, ctx.current)
        request.current = requestAnimationFrame(drawFrame)
    }, [])

    useEffect(() => {
        cpu.current.poke(16 + 0, 3)
        cpu.current.poke24(16 + 10, 160) // Set the virtual h / w
        cpu.current.poke24(16 + 13, 120)
        ctx.current = canvas.current.getContext('2d')
        display.current = new Display()
        request.current = requestAnimationFrame(drawFrame)
        return () => cancelAnimationFrame(request.current)
    }, [canvas.current])

    return (
        <canvas ref={canvas} width={640} height={480}></canvas>
    )
}