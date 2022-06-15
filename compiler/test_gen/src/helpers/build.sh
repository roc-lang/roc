zig build-obj wasm_linking_host_imports.zig
zig build-obj wasm_linking_test_host.zig
zig build-exe wasm_linking_test_host.o wasm_linking_host_imports.o
./wasm_linking_test_host
