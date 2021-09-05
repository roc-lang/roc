# Development backend for WebAssembly

This document offers a summary of how WebAssembly differs from other targets and how those differences matter for Roc.

## Stack machine vs register machine

Wasm's instruction set is based on a stack-machine VM. Whereas CPU instructions have named registers that they operate on, Wasm has no named registers at all. The instructions don't contain register names. Instructions can oly operate on whatever data is at the top of the stack.

For example the instruction `i64.add` takes two operands. It pops the top two arguments off the VM stack and pushes the result back.

In the [spec][spec-instructions], every instruction has a type signature! This is not something you would see for CPU instructions. The type signature for i64.add is `[i64 i64] → [i64]` because it pushes two i64's and pops an i64.

[spec-instructions]: https://webassembly.github.io/spec/core/appendix/index-instructions.html

This means that WebAssembly has a concept of type checking. When you load a .wasm file as a chunk of bytes into a Wasm runtime (like a browser or [wasmer](https://wasmer.io/)), the runtime will first _validate_ those bytes. They have some fast way of checking whether the types being pushed and popped are consistent. So if you try to do the i64.add instruction when you have floats on the stack, it will fail validation.

Note that the instruction makes no mention of any source or destination registers, because there is no such thing. It just pops two values and pushes one. (This architecture choice helps to keep WebAssembly programs quite compact. There are no extra bytes specifying source and destination registers.)

Implications of the stack machine for Roc:

- There is no such thing as register allocation, since there are no registers! There is no reason to maintain hashmaps of what registers are free or not. And there is no need to do a pass over the IR to find the "last seen" occurrence of a symbol in the IR. That means we don't need the `Backend` methods `scan_ast`, `scan_ast_call`, `set_last_seen`, `last_seen_map`, `free_map`, `free_symbols`, `free_symbol`, `set_free_map`.

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

If we run this code through the `wasm-opt` tool from the [binaryen toolkit](https://github.com/WebAssembly/binaryen#tools), all of the locals get optimised away. But this optimised version is harder to generate directly from the Mono IR.

```
  (func (;0;) (param i64 i64) (result i64)
    local.get 0
    local.get 1
    i64.add)    ; return can be implicit, like in Rust. Only really needed from inside a branch.
  (func (;1;) (result i64)
    i64.const 1
    i64.const 2
    call 0        ; the result of the first addition is at the top of the stack. No need to set a local and get it again.
    i64.const 4   ; no need to store the constant to a local and get it again
    call 0)
```

## Memory

WebAssembly programs have a "linear memory" for storing data, which is a block of memory assigned to it by the host. You can assign a min and max size to the memory, and the WebAssembly program can request 64kB pages from the host, just like a "normal" program would request pages from the OS. Addresses start at zero and go up to whatever the current size is. Zero is a perfectly normal address like any other, and dereferencing it is not a segfault. But addresses beyond the current memory size are out of bounds and dereferencing them will cause a panic.

The program has full read/write access to the memory and can divide it into whatever sections it wants. Most programs will want to do the traditional split of static memory, stack memory and heap memory.

The WebAssembly module structure includes a data section that will be copied into the linear memory at a specified offset on initialisation, so you can use that for string literals etc. But the division of the rest of memory into "stack" and "heap" areas is not a first-class concept. It is up to the compiler to generate instructions to do whatever it wants with that memory.

## Stack machine vs stack memory

**There are two entirely different meanings of the word "stack" that are relevant to the WebAssembly backend.** It's unfortunate that the word "stack" is so overloaded. I guess it's just a useful data structure. The worst thing is that both of them tend to be referred to as just "the stack"! We need more precise terms.

When we are talking about the instruction set, I'll use the term _machine stack_ or _VM stack_. This is the implicit data structure that WebAssembly instructions operate on. In the examples above, it's where `i64.add` gets its arguments and stores its result. I think of it as an abstraction over CPU registers, that WebAssembly uses in order to be portable and compact.

When we are talking about how we store values in _memory_, I'll use the term _stack memory_ rather than just "the stack". It feels clunky but it's the best I can think of.

Of course our program can use another area of memory as a heap as well. WebAssembly doesn't mind how you divide up your memory. It just gives you some memory and some instructions for loading and storing.

## Function calls

In WebAssembly you call a function by pushing arguments to the stack and then issuing a `call` instruction, which specifies a function index. The VM knows how many values to pop off the stack by examining the _type_ of the function. In our example earlier, `Num.add` had the type `[i64 i64] → [i64]` so it expects to find two i64's on the stack and pushes one i64 back as the result. Remember, the runtime engine will validate the module before running it, and if your generated code is trying to call a function at a point in the program where the wrong value types are on the stack, it will fail validation.

Function arguments are restricted to the four value types, `i32`, `i64`, `f32` and `f64`. If those are all we need, then there is _no need for any stack memory_, stack pointer, etc. We saw this in our example earlier. We just said `call 0`. We didn't need any instructions to create a stack frame with a return address, and there was no "jump" instruction. Essentially, WebAssembly has a first-class concept of function calls, so you don't build it up from lower-level primitives. You could think of this as an abstraction over calling conventions.

That's all great for primitive values but what happens when we want to pass more complex data structures between functions?

Well, remember, "stack memory" is not a special kind of memory in WebAssembly, it's just an area of our memory where we _decide_ that we want to implement a stack data structure. So we can implement it however we want. A good choice would be to make our stack frame look the same as it would when we're targeting a CPU, except without the return address (since there's no need for one). We can also decide to pass numbers through the machine stack rather than in stack memory, since that takes fewer instructions.

The only other thing we need is a stack pointer. On CPU targets, there's often have a specific "stack pointer" register. WebAssembly has no equivalent to that, but we can use a `global` variable.

The system I've outlined above is based on my experience of compiling C to WebAssembly via the Emscripten toolchain (which is built on top of clang). It's also in line with what the WebAssembly project describes [here](https://github.com/WebAssembly/design/blob/main/Rationale.md#locals).

## Structured control flow

One of the security features of WebAssembly is that it does not allow unrestricted "jumps" to anywhere you like. It does not have an instruction for that. All of the [control instructions][control-inst] can only implement "structured" control flow, and have names like `if`, `loop`, `block` that you'd normally associate with high-level languages. There are branch (`br`) instructions that can jump to labelled blocks within the same function, but the blocks have to be nested in sensible ways.

[control-inst]: https://webassembly.github.io/spec/core/syntax/instructions.html#control-instructions

Implications:

Roc, like most modern languages, is already enforcing structured control flow in the source program. Constructs from the Roc AST like `When`, `If` and `LetRec` all have corresponding Wasm constructs.

However the Mono IR converts this to jumps and join points, which are more of a Control Flow Graph than a tree. That doesn't map so directly to the Wasm structures. This is such a common issue for compiler back-ends that the WebAssembly compiler toolkit `binaryen` has an algorithm to solve it, called the "[Relooper][relooper]". We're not using `binaryen` right now. It's a C++ library, though it does have a (very thin and somewhat hard-to-use) [Rust wrapper][binaryen-rs].

[relooper]: https://github.com/WebAssembly/binaryen/wiki/Compiling-to-WebAssembly-with-Binaryen#cfg-api
[binaryen-rs]: https://crates.io/crates/binaryen

## Modules vs Instances

What's the difference between a Module and an Instance in WebAssembly?

Well, if I compare it to running a program on Linux, it's like the difference between an ELF binary and the executable image in memory that you get when you _load_ that ELF file. The ELF file is essentially a _specification_ for how to create the executable image. In order to start executing the program, the OS has to actually allocate a stack and a heap, and load the text and data. If you run multiple copies of the same program, they will each have their own memory and their own execution state. (More detail [here](https://wiki.osdev.org/ELF#Loading_ELF_Binaries)).

The Module is like the ELF file, and the Instance is like the executable image.

The Module is a _specification_ for how to create an Instance of the program. The Module says how much memory the program needs, but the Instance actually _contains_ that memory. In order to run the Wasm program, the VM needs to create an instance, allocate some memory for it, and copy the data section into that memory. If you run many copies of the same Wasm program, you will have one Module but many Instances. Each instance will have its own separate area of memory, and its own execution state.

## Modules and object files

TODO

https://webassembly.github.io/spec/core/binary/modules.html
