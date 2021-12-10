#include <stdio.h>

void main() {
    printf("Hello, I am a C program and I use libc.\n");
    printf("Would you like to use libc too, but just can't find the right path?\n");
    printf("Well, simply compile me from build.rs using Zig's --global-cache-dir,\n");
    printf("and have libc.a delivered right to your build directory!\n");
}
