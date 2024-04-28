require('esbuild').build({
    entryPoints: ['src/app.js', 'src/snippet_emulator_demo.jsx', 'src/forge_editor_demo.jsx'],
    bundle: true,
    minify: true,
    outdir: 'build',
    format: 'esm',
    loader: {
        '.wasm': 'file'
    }
}).catch(() => process.exit(1))