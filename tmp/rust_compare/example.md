# `starting_plants` — C vs Rust, optimized codegen

This is an apples-to-apples comparison of the machine code that C (`clang`) and Rust
(`rustc`) generate for the same function: build a heap-allocated list of 15 `Plant`s by
calling an external `random_plant` for each, then return a `{cap, ptr, len}` header.

Both versions are written to do exactly what Roc's compiled code does:

- a single direct allocation call,
- a null check that, on failure, calls a crash handler matching Roc's `RocOps.roc_crashed`
  ABI — `handle_oom(const RocCrashed *, void *env)`, noreturn,
- the `0..=14` loop driving an opaque `random_plant(i * 12)` call,
- results stored in place,
- the 3-word header (`{cap, ptr, len}`, same field order as Rust's `RawVec`) written once,
- returned via the aarch64 `sret` convention (`x8`),
- **zero refcounting.**

The point of the exercise: this is the instruction-level target the Roc pipeline should
hit once inlining + SpecConstr collapse the lazy `Stream` into a counted loop over a
pre-sized list and borrow inference removes the refcounts. At that point Roc would hand
LLVM this exact shape (modulo `malloc` being Roc's `roc_alloc` and the `random_plant` call
being the indirect hosted-vtable call).

---

## The C file (`plants.c`)

```c
#include <stdlib.h>
#include <stdint.h>

typedef struct { int32_t x; uint32_t type; } Plant;

extern Plant random_plant(int32_t seed);

typedef struct {
    size_t cap;
    Plant *ptr;
    size_t len;
} PlantVec;

// Matches Roc's RocOps.roc_crashed ABI: (const RocCrashed *, void *env), noreturn.
typedef struct { uint8_t *utf8_bytes; size_t len; } RocCrashed;
extern _Noreturn void handle_oom(const RocCrashed *crashed, void *env);

#define PLANT_COUNT 15

PlantVec starting_plants(void) {
    Plant *ptr = malloc(PLANT_COUNT * sizeof(Plant));
    if (ptr == NULL) {
        static const char msg[] = "Ran out of memory!";
        RocCrashed crashed = { (uint8_t *)msg, sizeof(msg) - 1 };
        handle_oom(&crashed, NULL);
    }
    for (int i = 0; i < PLANT_COUNT; i++) {
        ptr[i] = random_plant(i * 12);
    }
    return (PlantVec){ .ptr = ptr, .len = PLANT_COUNT, .cap = PLANT_COUNT };
}
```

## The Rust file (`plants_roc.rs`)

```rust
#![crate_type = "lib"]

use std::os::raw::c_void;
use std::ptr;

#[repr(C)]
pub struct Plant {
    pub x: i32,
    pub r#type: u32,
}

// Same memory order as the C PlantVec / Rust's RawVec: { cap, ptr, len }.
#[repr(C)]
pub struct PlantVec {
    pub cap: usize,
    pub ptr: *mut Plant,
    pub len: usize,
}

// Matches Roc's RocOps.roc_crashed ABI: (const RocCrashed *, void *env), noreturn.
#[repr(C)]
pub struct RocCrashed {
    pub utf8_bytes: *mut u8,
    pub len: usize,
}

extern "C" {
    fn malloc(size: usize) -> *mut c_void;
    fn random_plant(seed: i32) -> Plant;
    fn handle_oom(crashed: *const RocCrashed, env: *mut c_void) -> !;
}

const PLANT_COUNT: usize = 15;

#[no_mangle]
pub extern "C" fn starting_plants() -> PlantVec {
    unsafe {
        let ptr = malloc(PLANT_COUNT * core::mem::size_of::<Plant>()) as *mut Plant;
        if ptr.is_null() {
            static MSG: &[u8] = b"Ran out of memory!";
            let crashed = RocCrashed {
                utf8_bytes: MSG.as_ptr() as *mut u8,
                len: MSG.len(),
            };
            handle_oom(&crashed, ptr::null_mut());
        }
        let mut i: i32 = 0;
        while (i as usize) < PLANT_COUNT {
            ptr.add(i as usize).write(random_plant(i * 12));
            i += 1;
        }
        PlantVec {
            cap: PLANT_COUNT,
            ptr,
            len: PLANT_COUNT,
        }
    }
}
```

---

## How to compile and disassemble (reproduce on aarch64 macOS)

```sh
# C
clang -O3 -c plants.c -o plants_c.o

# Rust
rustc -C opt-level=3 -C panic=abort --emit obj plants_roc.rs -o plants_roc.o

# Disassemble just starting_plants from each
otool -tVj plants_c.o   | sed -n '/_starting_plants:/,/^_[a-z]/p'
otool -tVj plants_roc.o | sed -n '/_starting_plants:/,/^_[a-z]/p'
```

(Note: `_Noreturn` on `handle_oom` and Rust's `-> !` keep the cold path from forcing any
spills onto the hot path. `-C panic=abort` avoids landing pads. macOS aarch64 mandates a
frame pointer, so don't pass `-fomit-frame-pointer` to clang if you want a byte-for-byte
match — both keep the `add x29, sp, #0x20` setup.)

---

## How they compare

**68 instructions each. The success path is byte-for-byte identical, down to register
allocation and instruction addresses.** Both emit:

```
sub  sp, sp, #0x30
stp  x20, x19, [sp, #0x10]
stp  x29, x30, [sp, #0x20]
add  x29, sp, #0x20
mov  x19, x8                 ; save sret pointer
mov  w0, #0x78               ; 120 = 15 * sizeof(Plant)
bl   malloc
cbz  x0, <oom>               ; null check
mov  x20, x0                 ; x20 = data ptr
mov  w0, #0x0    ; bl random_plant ; str x0, [x20]        ; seed 0   -> [0]
mov  w0, #0xc    ; bl random_plant ; str x0, [x20, #0x8]  ; seed 12  -> [1]
mov  w0, #0x18   ; bl random_plant ; str x0, [x20, #0x10] ; seed 24  -> [2]
...                                                        ; 15 calls total, fully unrolled
mov  w0, #0xa8   ; bl random_plant ; str x0, [x20, #0x70] ; seed 168 -> [14]
mov  w8, #0xf                ; 15
stp  x8, x20, [x19]          ; cap@0, ptr@8
str  x8, [x19, #0x10]        ; len@16
ldp  x29, x30, [sp, #0x20]
ldp  x20, x19, [sp, #0x10]
add  sp, sp, #0x30
ret
```

Note the loop is **fully unrolled into 15 straight-line `random_plant` calls** with the
`i * 12` seeds constant-folded (`0, 12, 24, ..., 168` = `0x0 .. 0xa8`), each result stored
directly into its slot. One allocation, no per-element bookkeeping, no refcounting.

### The only difference: 2 instructions on the cold (OOM) path

Both build the identical `RocCrashed { msg_ptr, 18 }` on the stack and call
`handle_oom(&crashed, NULL)`. They differ only in how those 16 bytes are materialized:

```
C:                                  Rust:
  adrp x8, msg                        adrp x8, msg
  add  x8, x8, #0                     add  x8, x8, #0
  ldr  q0, [x8]   ; {ptr,18} const    mov  w9, #0x12      ; len = 18
  str  q0, [sp]                       stp  x8, x9, [sp]
  mov  x0, sp                         mov  x0, sp
  mov  x1, #0x0   ; env = NULL        mov  x1, #0x0       ; env = NULL
  bl   handle_oom                     bl   handle_oom
```

clang folds `{msg_ptr, 18}` into a 16-byte literal-pool constant and does a vector
load/store; rustc keeps the length as an immediate and uses `stp`. Same instruction count,
same effect, entirely off the hot path. Nothing observable distinguishes the two.

### Notes on what made them match

- **Struct field order matters.** With `{ptr, len, cap}` (len/cap adjacent), clang fused the
  two equal `15`s into a `dup.2d` + `stur q0` SIMD store — which was actually *one
  instruction longer* than the `stp`+`str` you get from Rust's `{cap, ptr, len}` order
  (where the pointer sits between cap and len, so there's no adjacent equal pair to
  vectorize). Using `{cap, ptr, len}` makes both emit the identical `stp`+`str`.
- **Roc's own layouts** are `RocList { ptr, len, cap }` and `RocStr { ptr, cap, len }` — both
  keep the pointer first and the two size fields adjacent (friendlier than Rust's `Vec`),
  but Roc stores `capacity << 1` (low bit flags a seamless slice), so the stored cap is
  never equal to len and the `dup` fusion can't fire there regardless.
