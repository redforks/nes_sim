build-release:
	cargo rustc --release --bin nes_run -- -C target-cpu=native
