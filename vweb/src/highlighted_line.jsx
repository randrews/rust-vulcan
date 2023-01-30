import React from 'react'

const keywords = {
    '.db': 'directive',
    '.org': 'directive',
    '.equ': 'directive',
    '#if': 'macro',
    '#unless': 'macro',
    '#while': 'macro',
    '#until': 'macro',
    '#do': 'macro',
    '#else': 'macro',
    '#end': 'macro'
}

export default function({ line }) {
    let remainder = line.trim(), // The part of the line we haven't processed
        current = '', // The characters that aren't part of a recognized token that we're building up
        idx = 0, // A counter for unique keys
        label_directive = false // Whether this line has a label definition or directive on it
    const tokens = [] // The list of spans we're building up

    // Push a span containing the "current" string
    const pushCurrent = () => {
        if (current !== '') { tokens.push(<span key={`token-${idx++}`}>{current}</span>) }
        current = ''
    }

    // Push a recognized token with a classname
    const pushToken = (css, tok) => {
        pushCurrent()
        tokens.push(<span className={css} key={`token-${idx++}`}>{tok}</span>)
        remainder = remainder.slice(tok.length)
    }

    // Consume things from the remainder of the line. The general idea here is that we look at the start of the str
    // and see if it's a recognizable token. If not, add a single char to `current` and keep going. If so, then push
    // `current`, push that token, and keep going.
    // This works but only just: everything we highlight (directives, macros, label _declarations,_ numbers, strings,
    // and comments) is recognizable that way and can't be a substring of anything else. We could not, for example,
    // recognize label _usages_ this way because we can't tell from the first character that it is one
    while (remainder !== '') {
        let any = false // Whether any special thing matched

        if (remainder[0] === ';') { // If it's a comment, consume the rest
            pushToken('comment', remainder)
            remainder = ''
            any = true
        }

        if (remainder[0] === '"') { // Is it a string?
            let [str, newRemainder] = readQuotedString(remainder)
            pushToken('string', str)
            remainder = newRemainder
            any = true
        }

        // Try to match number patterns
        const num = remainder.match(/^(0x[0-9a-fA-F]+)/) ||
            remainder.match(/^(0b[10]+)/) ||
            remainder.match(/^(-?\d+)/)
        if (num && num[1]) { // Is it a number?
            pushToken('number', num[1])
            any = true
        }

        // Any sort of keyword?
        Object.keys(keywords).forEach((keyword) => {
            if (remainder.startsWith(keyword)) {
                pushToken(keywords[keyword], keyword)
                label_directive = true
                any = true
            }
        })

        // Labels!
        const lbl = remainder.match(/^([a-zA-Z\d_$]+:)/)
        if (lbl && lbl[1]) {
            pushToken('label', lbl[1])
            any = true
            label_directive = true
        }

        if (!any) { // Nothing better matched, eat a char and keep going
            current += remainder[0]
            remainder = remainder.slice(1)
        }
    }

    pushCurrent()

    return <>{label_directive ? '' : <>&nbsp;&nbsp;</>}{tokens}</>
}

// A cheap way to indent lines: if the first token is a macro, directive, or label declaration, indent it two spaces
export function indent(str) {
    str = str.trim()
    if (str.match(/^#[a-z]+/) ||
        str.match(/^\.[a-z]+/) ||
        str.match(/^[a-zA-Z\d]+:/)) {
        return str
    } else {
        return '  ' + str
    }
}

// If the start of this line is a quoted string, return that + the remainder (in an escape-aware way). Otherwise
// just return empty string + the entire line
function readQuotedString(line) {
    if (line[0] === '"') {
        for (let n = 1; n < line.length; n++) {
            if (line[n] === '"' && line[n-1] !== '\\') {
                return [line.slice(0, n+1), line.slice(n+1)]
            }
        }
    }
    return ['', line]
}