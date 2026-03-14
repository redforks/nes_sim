# NES CPU Test Runner

CLI tool for running CPU test ROMs with exit code detection.

## Running Tests

```bash
# Run all CPU tests via make (uses test ROMs from nes-test-roms/)
make cpu-test

# Run individual test ROM
target/release/nes_cpu_test -f path/to/test.nes

# Run unit tests
cargo test --test <test_name>
```

## Building

```bash
cargo build --release    # Build the test runner
cargo build              # Build in debug mode
```
