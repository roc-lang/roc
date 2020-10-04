#!/bin/bash

clang -c host/src/host.c -o c_host.o
rustc host/src/host.rs -o rust_host.o
ld -r c_host.o rust_host.o -o host.o

rm -f c_host.o
rm -f rust_host.o
