import React, { useState, useCallback, useEffect, useRef } from 'react'
import { WasmCPU } from '../pkg/vweb.js'

export default function({}) {
    const canvas = useRef(null)
    const [ctx, setCtx] = useState(null)
    const [cpu, _setCpu] = useState(new WasmCPU())

    useEffect(() => {
        const c = canvas.current.getContext('2d')
        setCtx(c)
        cpu.draw_frame(c)
    }, [canvas.current])
    return (
        <canvas ref={canvas} width={640} height={480}></canvas>
    )
}