#!/bin/bash

clang -c host.c -o c_host.o
rustc host.rs -o rust_host.o

# Linking - see https://github.com/rtfeldman/roc/pull/554#discussion_r496365925
# for discussion and further references
#
# TODO will eventually need compiler_rt from -lgcc or something - see https://github.com/rtfeldman/roc/pull/554#discussion_r496370840
ld -r -static \
  -L /usr/lib/x86_64-linux-musl \
  -L /usr/lib/llvm-10/lib \
  -L /usr/lib/x86_64-linux-gnu \
  -dynamic-linker /lib64/ld-linux-x86-64.so.2 \
  /usr/lib/x86_64-linux-gnu/crti.o \
  /usr/lib/x86_64-linux-gnu/crtn.o \
  /usr/lib/x86_64-linux-gnu/Scrt1.o \
  -lc \
  -lm \
  -lpthread \
  -ldl \
  -lrt \
  -lutil \
  -lc++abi \
  -lunwind \
  c_host.o \
  rust_host.o \
  -o host.o

rm -f c_host.o
rm -f rust_host.o
