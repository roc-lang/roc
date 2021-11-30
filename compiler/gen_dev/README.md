# Dev Backend

The dev backend is focused on generating decent binaries extremely fast.
It goes from Roc's [mono ir](https://github.com/rtfeldman/roc/blob/trunk/compiler/mono/src/ir.rs) to an object file ready to be linked.

## General Process

The backend is essentially defined as two recursive match statement over the mono ir.
The first pass is used to do simple linear scan lifetime analysis.
In the future it may be expanded to add a few other quick optimizations.
The second pass is the actual meat of the backend that generates the byte buffer of output binary.
The process is pretty simple, but can get quite complex when you have to deal with memory layouts, function calls, and multiple architectures.

## Core Abstractions

This library is built with a number of core traits/generic types that may look quite weird at first glance.
The reason for all of the generics and traits is to allow rust to optimize each target specific backend.
Instead of every needing an `if linux ...` or `if arm ...` statement within the backend,
rust should be abled compile each specific target (`linux-arm`, `darwin-x86_64`, etc) as a static optimized backend without branches on target or dynamic dispatch.

**Note:** links below are to files, not specific lines. Just look up the specific type in the file.

### Backend

[Backend](https://github.com/rtfeldman/roc/blob/trunk/compiler/gen_dev/src/lib.rs) is the core abstraction.
It understands Roc's mono ir and some high level ideas about the generation process.
The main job of Backend is to do high level optimizatons (like lazy literal loading) and parse the mono ir.
Every target specific backend must implement this trait.

### Backend64Bit

[Backend64Bit](https://github.com/rtfeldman/roc/blob/trunk/compiler/gen_dev/src/generic64/mod.rs) is more or less what it sounds like.
It is the backend that understands 64 bit architectures.
Currently it is the only backend implementation, but a 32 bit implementation will probably come in the future.
This backend understands that the unit of data movement is 64 bit.
It also knows about things common to all 64 bit architectures (general purpose registers, stack, float regs, etc).

If you look at the signiture for Backend64Bit, it is actually quite complex.
Backend64Bit is generic over things like the register type, assembler, and calling convention.
This enables to backend to support multiple architectures and operating systems.
For example, the `windows-x86_64` would use the x86 register set, the x86 assembler, and the x86 windows calling convention.
`darwin-x86_64` and `linux-x86_64` would use the same register set and assembler, but they would use the system v amd64 abi calling convention.
Backend64Bit is generic over these types instead of containing these types within it's struct to avoid the cost of dynamic dispatch.

### Assembler

[Assembler](https://github.com/rtfeldman/roc/blob/trunk/compiler/gen_dev/src/generic64/mod.rs) is the trait for generating assembly bytes.
It defines a set of RISC-like assembly calls that must be implemented for each architecture.
A lot of these calls may not map one to one with actual assembly instructions for each architecture.
Instead, they are a general abstraction over functionality shared between all architectures.
This will grow regularly as more Roc builtins are added.
Here are example implementations for [arm](https://github.com/rtfeldman/roc/blob/trunk/compiler/gen_dev/src/generic64/aarch64.rs) and [x86_64](https://github.com/rtfeldman/roc/blob/trunk/compiler/gen_dev/src/generic64/x86_64.rs).

### CallConv

[CallConv](https://github.com/rtfeldman/roc/blob/trunk/compiler/gen_dev/src/generic64/mod.rs) is the abstraction over calling conventions.
It deals with register and stack specific information related to passing and returning arguments.
Here are example implementations for [arm](https://github.com/rtfeldman/roc/blob/trunk/compiler/gen_dev/src/generic64/aarch64.rs) and [x86_64](https://github.com/rtfeldman/roc/blob/trunk/compiler/gen_dev/src/generic64/x86_64.rs).

## Adding New Features

Adding a new builtin to the dev backend can be pretty simple.
Here is [an example](https://github.com/rtfeldman/roc/pull/893/files) of adding `Num.Sub`.

This is the general procedure I follow with some helpful links:

1. Find a feature that is just n+1.
   For example, since we already have integers, adding a builtin that functions on them should be n+1.
   On the other hand, since we don't yet have booleans/conditionals, adding if statements may not yet be n+1.
   A good place to look for missing features is in the test files for generation in [test_gen](https://github.com/rtfeldman/roc/tree/trunk/compiler/test_gen). Any test that is not enabled for the `gen-dev` feature still needs to be added to the dev backend. Eventually all features should be enabled for the dev backend.
1. Pick/write the simplest test case you can find for the new feature.
   Just add `feature = "gen-dev"` to the `cfg` line for the test case.
1. Uncomment the code to print out procedures [from here](https://github.com/rtfeldman/roc/blob/b03ed18553569314a420d5bf1fb0ead4b6b5ecda/compiler/test_gen/src/helpers/dev.rs#L76) and run the test.
   It should fail and print out the mono ir for this test case.
   Seeing the actual mono ir tends to be very helpful for complex additions.
1. Generally it will fail in one of the match statements in the [Backend](https://github.com/rtfeldman/roc/blob/trunk/compiler/gen_dev/src/lib.rs) trait.
   Add the correct pattern matching and likely new function for your new builtin.
   This will break the compile until you add the same function to places that implement the trait,
   like [Backend64Bit](https://github.com/rtfeldman/roc/blob/trunk/compiler/gen_dev/src/generic64/mod.rs).
1. Keep following the chain down.
   To implement the function in Backend64Bit, you may need to add new assembly calls.
   Feel free to ignore backends that aren't x86_64 for now and just add `unimplemented!`.
   See the helpful resources section below for guides on figuring out assembly bytes.
1. Hopefully at some point everything compiles and the test is passing.
   If so, yay. Now add more tests for the same feature and make sure you didn't miss the edge cases.
1. If things aren't working, reach out on zulip. Get advice, maybe even pair.
1. Make a PR.

## Helpful Resources

- [Compiler Explorer](godbolt.org) -
  Generates assembly from most languages.
  Really good for getting a reference for what is required to do something.
  Can answer questions like "how would x be implemented in arm assembly?"
- [objdump](https://www.tutorialspoint.com/unix_commands/objdump.htm) -
  Super super useful commandline tool.
  Lets you inspect exactly what is generated in a binary.
  Can inspect assembly, relocations, and more.
  I use this all the time for debugging and inspecting C sample apps.
  May write a larger tutorial for this because it can be seriosly helpful.
  As a note, when dealing with relocatoins, please make sure to compile with PIC.
- [Online Assembler](https://defuse.ca/online-x86-assembler.htm#disassembly) -
  Useful for seeing the actual bytes generated by assembly instructions.
  A lot of time it gives on out of multiple options because x86_64 has many ways to do things.
  Also, sometimes it doesn't seem to generate things quite as you expect.
- [Alternative Online Assembler](http://shell-storm.org/online/Online-Assembler-and-Disassembler/) -
  Like previous but with more architecture options.
- [x86 and amd64 instruction reference](https://www.felixcloutier.com/x86/) -
  Great for looking up x86_64 instructions and there bytes.
  Definitely missing information if you aren't used to reading it.
- [Intel 64 ISA Reference](https://software.intel.com/content/dam/develop/public/us/en/documents/325383-sdm-vol-2abcd.pdf) -
  Super dense manual.
  Contains everything you would need to know for x86_64.
  Also is like 2000 pages.
- [ARM architecture reference manual](https://developer.arm.com/documentation/ddi0487/latest/) -
  Same thing as the intel manual, but for ARM.
  It is huge, but quite useful.
  Links in the pdf make it easy to jump around and understand our arm enum.
- [A ToC of the 20 part linker essay](https://lwn.net/Articles/276782/) -
  Lots of information on linkers by the author of the gold linker.
- If there is anything else basic that you want to know,
  there is a good chance it is include in lectures from compiler courses.
  Definitely look at some of the free moocs, lectures, or youtube class recordings on the subject.
- If you have any specific questions feel free to ping Brendan Hansknecht on zulip.
- If you have any other resource that you find useful, please add them here.
