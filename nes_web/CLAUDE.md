# NES Web (WASM)

WebAssembly build for browser-based NES emulation.

## Building

```bash
# From project root
make wasm-debug-build    # Build WASM package

# Or directly
wasm-pack build --dev
```

## Development

```bash
# Start webpack dev server
make web-start

# Or directly
cd ../www && npm start
```

## Notes

- Builds to `pkg/` directory with wasm bindings
- Used by the `www/` frontend
