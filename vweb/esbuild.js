require('esbuild').build({
    entryPoints: ['src/app.js', 'src/snippet_emulator_demo.js'],
    bundle: true,
    outdir: 'build',
    format: 'esm',
    loader: {
        '.wasm': 'file'
    }
}).catch(() => process.exit(1))