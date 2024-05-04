export default {
    name: 'forge',

    startState: function () {
        // asm is "have we seen the asm keyword." The first time we see braces after that keyword (ignoring
        // strings, because string literals in an asm arg block is legal) we start highlighting it as asm.
        // The current state is just the current state, null means normal.
        return {current: null, asm: false}
    },

    token: function (stream, state) {
        if (stream.eatSpace()) return null

        if (state.current === null) {
            let word
            if (word = stream.match(/^[a-zA-Z_$][a-zA-Z0-9_$]*/)) {
                // This is either a keyword or an identifier.
                // All the keywords are valid names, so, check that:
                if (word[0].match(/^fn|var|new|static|asm|return|if|while|repeat|global|const|peek|poke|break$/)) {
                    if (word[0] === 'asm') {
                        state.asm = true
                    } // This is the start of an asm control structure
                    return 'keyword'
                } else {
                    return 'variable'
                }
            } else if (stream.match(/^-?((0b[01]+)|(0x[0-9a-fA-F]+)|(0o[0-7]+)|0|([1-9][0-9]*?))/)) {
                // All the number formats. Unary minus is just an operator
                return 'number'
            } else if (stream.match(/^"/)) {
                // Open quote to start a string
                state.current = 'string'
                return 'string'
            } else if (stream.match(/^{/)) {
                // Open brace
                if (state.asm) {
                    // This is the start of the _actual_ asm block, kick us into asm mode and gooooo
                    state.current = 'asm'
                    state.asm = false // clear the flag
                    return 'quote'
                } else {
                    // A normal open brace, just highlight it. If we do indentation stuff it goes here.
                    return 'strong'
                }
            } else if (stream.match(/^[};]/)) {
                // Close brace and semicolon get bolded.
                return 'strong'
            } else if (stream.match(/^\/\/.*/)) {
                // Normal line comment
                return 'comment'
            } else if (stream.match(/^\/\*/)) {
                // Start of a block comment, switch our mode
                state.current = 'comment'
                return 'comment'
            } else if (stream.match(/^&&|\|\||==|!=|>=|<=|[-+*/%&|^><=]/)) {
                // These operators have to be after the comment check
                return 'operator'
            } else {
                // Dunno what this is, so just return content
                stream.next()
                return 'content'
            }
        } else if (state.current === 'comment') {
            // We're in a block comment
            if (stream.match(/^\*\//)) {
                // End of a block comment, clear the state
                state.current = null
            } else {
                // Have to move forward
                stream.next()
            }
            return 'comment' // No matter whether we clear the state or not, it's still currently a comment
        } else if (state.current === 'string') {
            // We're in a string
            if (stream.match(/^"/)) {
                // We're no longer in a string
                state.current = null
            } else {
                // If the next char is a backslash...
                stream.eat(/^\\/)
                stream.next() // Advance past the _next_ char, skip whatever the escape is.
            }
            return 'string'
        } else if (state.current === 'asm') {
            // We're in an asm block
            if (stream.match(/^}/)) {
                // asm blocks end on close-brace, whether quoted or not! Forge is unaware of any brace rules in
                // the assembler. This is a reality in the compiler as well, not just this highlighter.
                state.current = null
            } else {
                // If it's not a brace move forward anyway
                stream.next()
            }
            return 'quote'
        }
    }
}