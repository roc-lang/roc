# Development backend for WebAssembly

This document offers a summary of how WebAssembly differs from other targets and how those differences matter for Roc.

## Stack machine vs register machine

Wasm's instruction set is based on a stack-machine VM. Whereas CPU instructions have named registers that they operate on, Wasm has no named registers at all. The instructions don't contain register names. Instructions can oly operate on whatever data is at the top of the stack.

For example the instruction `i64.add` takes two operands. It pops the top two arguments off the VM stack and pushes the result back.

In the [spec][spec-instructions], every instruction has a type signature! This is not something you would see for CPU instructions. The type signature for i64.add is `[i64 i64]→[i64]` because it pushes two i64's and pops an i64.

[spec-instructions]: https://webassembly.github.io/spec/core/appendix/index-instructions.html

This means that WebAssembly has a concept of type checking. When you load a .wasm file as a chunk of bytes into a Wasm runtime (like a browser or [wasmer](https://wasmer.io/)), the runtime will first _validate_ those bytes. They have some fast way of checking whether the types being pushed and popped are consistent. So if you try to do the i64.add instruction when you have floats on the stack, it will fail validation.

Note that the instruction makes no mention of any source or destination registers, because there is no such thing. It just pops two values and pushes one.


Implications of the stack machine for Roc:

- There is no such thing as register allocation, since there are no registers! There is no reason to maintain hashmaps of what registers are free or not. And there is not much point in doing a pass over the IR to find the "last seen" occurrence of a symbol in the IR. That means we don't need the `Backend` methods `scan_ast`, `scan_ast_call`, `set_last_seen`, `last_seen_map`, `free_map`, `free_symbols`, `free_symbol`, `set_free_map`.

- There is no random access to the stack. All instructions operate on the data at the _top_ of the stack. There is no instruction that says "get the value at index 17 in the stack". If such an instruction did exist, it wouldn't be a stack machine. And there is no way to "free up some of the slots in the stack". You have to consume the stuff at the top, then the stuff further down. However Wasm has a concept of local variables, which do allow random access. See below.

## Local variables

WebAssembly functions can have any number of local variables. They are declared at the beginning of the function, along with their types (just like C). WebAssembly has 4 value types: `i32`, `i64`, `f32`, `f64`.

In this backend, each symbol in the Mono IR gets one WebAssembly local. To illustrate, let's translate a simple Roc example to WebAssembly text format.
The WebAssembly code below is completely unoptimised and uses far more locals than necessary. But that does help to illustrate the concept of locals.

```
app "test" provides [ main ] to "./platform"

main =
    1 + 2 + 4
```

The Mono IR contains two functions, `Num.add` and `main`, so we generate two corresponding WebAssembly functions.

```
  (func (;0;) (param i64 i64) (result i64)   ; declare function index 0 (Num.add) with two i64 parameters and an i64 result
    local.get 0              ; load param 0                                    stack=[param0]
    local.get 1              ; load param 1                                    stack=[param0, param1]
    i64.add                  ; pop two values, add, and push result            stack=[param0 + param1]
    return)                  ; return the value at the top of the stack

  (func (;1;) (result i64)   ; declare function index 1 (main) with no parameters and an i64 result
    (local i64 i64 i64 i64)  ; declare 4 local variables, all with type i64, one for each symbol in the Mono IR
    i64.const 1              ; load constant of type i64 and value 1                stack=[1]
    local.set 0              ; store top of stack to local0                         stack=[]     local0=1
    i64.const 2              ; load constant of type i64 and value 2                stack=[2]    local0=1
    local.set 1              ; store top of stack to local1                         stack=[]     local0=1  local1=2
    local.get 0              ; load local0 to top of stack                          stack=[1]    local0=1  local1=2
    local.get 1              ; load local1 to top of stack                          stack=[1,2]  local0=1  local1=2
    call 0                   ; call function index 0 (which pops 2 and pushes 1)    stack=[3]    local0=1  local1=2
    local.set 2              ; store top of stack to local2                         stack=[]     local0=1  local1=2  local2=3
    i64.const 4              ; load constant of type i64 and value 4                stack=[4]    local0=1  local1=2  local2=3
    local.set 3              ; store top of stack to local3                         stack=[]     local0=1  local1=2  local2=3  local3=4
    local.get 2              ; load local2 to top of stack                          stack=[3]    local0=1  local1=2  local2=3  local3=4
    local.get 3              ; load local3 to top of stack                          stack=[3,4]  local0=1  local1=2  local2=3  local3=4
    call 0                   ; call function index 0 (which pops 2 and pushes 1)    stack=[7]    local0=1  local1=2  local2=3  local3=4
    return)                  ; return the value at the top of the stack
```

If we run this code through the `wasm-opt` tool from the [binaryen toolkit](https://github.com/WebAssembly/binaryen/wiki/Compiling-to-WebAssembly-with-Binaryen), all of the locals get optimised away. But this optimised version is harder to generate directly from the Mono IR.

```
  (func (;0;) (type 0) (param i64 i64) (result i64)
    local.get 0
    local.get 1
    i64.add)    ; return can be implicit, like in Rust. Only really needed from inside a branch.
  (func (;1;) (type 1) (result i64)
    i64.const 1
    i64.const 2
    call 0
    i64.const 4
    call 0)
```

## Stack machine vs stack memory
TODO

## Structured control flow
TODO

## Modules and objects
TODO

https://webassembly.github.io/spec/core/binary/modules.html
