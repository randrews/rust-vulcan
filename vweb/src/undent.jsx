// Remove an equal amount of leading whitespace from every line in a string,
// such that some lines have zero leading whitespace. Also remove all trailing whitespace.
export function undent(str) {
    const isBlank = (ln) => ln.match(/^\s*$/)
    const stripTrailingWs = ln => ln.match(/^(.*\S)\s*$/) // crashes on all-ws lines

    const leadingWhitespaceCount = (ln) => {
        const leadingWs = ln.match(/^(\s*)\S/)
        return leadingWs ? leadingWs[1].length : 0
    }

    const setLeadingWhitespace = (ln, count) => {
        const text = ln.match(/^\s*(\S.*)$/)[1]
        return `${(' ').repeat(count)}${text}`
    }

    if (isBlank(str)) {
        return ''
    } // Degenerate case

    const lines = str.split(/\n/)

    if (lines.size === 1) { // Another degenerate case
        return setLeadingWhitespace(stripTrailingWs(lines[0]), 0)
    }

    const leading = []
    lines.forEach((line) => {
        if (!isBlank(line)) {
            leading.push(leadingWhitespaceCount(line))
        }
    })
    const toRemove = Math.min(...leading)

    const cleanLines = lines.map((line) => {
        if (isBlank(line)) {
            return ''
        } else {
            const current = leadingWhitespaceCount(line)
            return setLeadingWhitespace(line, current - toRemove)
        }
    })

    return cleanLines.join('\n')
}