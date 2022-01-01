#include <stdio.h>

void main() {
    printf("Hello, I am a C program and I use libc.\n");
    printf("If you compile me, you'll compile libc too. That's handy for cross-compilation including Wasm.\n");
    printf("Use `zig build-exe` with `--global-cache-dir my/build/directory` to put libc.a where you want it.\n");
}
