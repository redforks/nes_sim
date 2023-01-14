cpu-test:
	cd nes_cpu_test; cargo run --release -- --quiet

wasm-build:
	cd nes_web; wasm-pack build


