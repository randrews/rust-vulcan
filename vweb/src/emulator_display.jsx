import React, { useCallback, useEffect, useRef } from 'react'
import { WasmCPU, Display } from '../pkg/vweb.js'

export default function({}) {
    const canvas = useRef(null)
    const cpu = useRef(new WasmCPU())
    const ctx = useRef(null)
    const display = useRef(null)
    const request = useRef(null)

    const drawFrame = useCallback(() => {
        display.current.draw(cpu.current, ctx.current)
        request.current = requestAnimationFrame(drawFrame)
    }, [])

    useEffect(() => {
        ctx.current = canvas.current.getContext('2d')
        display.current = new Display()
        request.current = requestAnimationFrame(drawFrame)
        return () => cancelAnimationFrame(request.current)
    }, [canvas.current])

    return (
        <canvas ref={canvas} width={640} height={480}></canvas>
    )
}