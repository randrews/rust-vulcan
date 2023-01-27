require('esbuild').build({
    entryPoints: ['src/app.js', 'src/snippet_emulator_demo.jsx'],
    bundle: true,
    outdir: 'build',
    format: 'esm',
    loader: {
        '.wasm': 'file'
    }
}).catch(() => process.exit(1))