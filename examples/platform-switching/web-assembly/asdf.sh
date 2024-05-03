rm -rf zig-cache
rm -rf zig-out
rm -rf *.o 
rm -rf *.dylib 
rm -rf *.ll 
rm -rf *.bc
rm -rf *.wasm

# compile host into a LLVM .bc file
# zig build-obj -target wasm32-wasi-musl -lc -rdynamic -femit-llvm-ir=platform/host.bc host/main.zig

# # compile app into a LLVM .ll file
# roc build --no-link --emit-llvm-ir rocLovesWebAssembly.roc                               

# # link together host and app
# zig build-exe -target wasm32-wasi-musl -lc platform/host.bc rocLovesWebAssembly.ll
# # ```
# error: wasm-ld: 
#     note: defined as (i32, i32, i32) -> i32 in /Users/luke/.cache/zig/o/e1ad72288e7681c39fb42e87c2506cfb/libcompiler_rt.a(/Users/luke/.cache/zig/o/e1ad72288e7681c39fb42e87c2506cfb/libcompiler_rt.a.o)
#     note: defined as (i32, i32, i64) -> i32 in /Users/luke/.cache/zig/o/8e32b79d2f74ce8703ec76103c3fa0de/rocLovesWebAssembly.o
# error: wasm-ld: 
#     note: defined as (i32, i32, i32) -> i32 in /Users/luke/.cache/zig/o/e1ad72288e7681c39fb42e87c2506cfb/libcompiler_rt.a(/Users/luke/.cache/zig/o/e1ad72288e7681c39fb42e87c2506cfb/libcompiler_rt.a.o)
#     note: defined as (i32, i32, i64) -> i32 in /Users/luke/.cache/zig/o/8e32b79d2f74ce8703ec76103c3fa0de/rocLovesWebAssembly.o
# error: wasm-ld: /Users/luke/.cache/zig/o/8e32b79d2f74ce8703ec76103c3fa0de/rocLovesWebAssembly.o: undefined symbol: setjmp
# error: wasm-ld: /Users/luke/.cache/zig/o/8e32b79d2f74ce8703ec76103c3fa0de/rocLovesWebAssembly.o: undefined symbol: longjmp
# ```


#   "wasm32-freestanding-musl",
#   "wasm32-wasi-musl",