app [main] { pf: platform "zig-platform/main.roc" }

# To run:
# cd examples/platform-switching/zig-platform
# mkdir glue
# cp crates/compiler/builtins/bitcode/src/* ./glue
# cd -
# roc --build-host --suppress-build-host-warning roc_loves_zig.roc

main = "Roc <3 Zig!\n"
