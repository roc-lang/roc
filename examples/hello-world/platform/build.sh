#!/bin/bash

# compile c_host.o and rust_host.o
clang -c host.c -o c_host.o
rustc host.rs -o rust_host.o

# link them together into host.o
ld -r c_host.o rust_host.o -o host.o

# clean up
rm -f c_host.o
rm -f rust_host.o
