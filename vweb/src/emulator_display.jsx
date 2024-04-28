import React, { useCallback, useEffect, useRef, useMemo } from 'react'
import { Display } from '../pkg'

export default function({ cpu }) {
    const canvas = useRef(null)
    const ctx = useRef(null)
    const display = useMemo(() => new Display(), [])
    const request = useRef(null)

    const drawFrame = useCallback(() => {
        display.draw(cpu, ctx.current)
        request.current = requestAnimationFrame(drawFrame)
    }, [display, cpu])

    useEffect(() => {
        ctx.current = canvas.current.getContext('2d')
        request.current = requestAnimationFrame(drawFrame)
        return () => cancelAnimationFrame(request.current)
    }, [canvas.current, drawFrame])

    return (
        <div className='display'>
            <canvas ref={canvas} width={640} height={480}></canvas>
        </div>
    )
}