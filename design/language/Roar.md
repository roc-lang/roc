# Roc Optimizable Abstract Representation (ROAR)

> [!WARNING]
> This assumes familiarity with concepts like the stack, registers

Roc Optimizable Abstract Representation (ROAR) is proposed part of the `dev` backend to be between the `mono` bytecode and the machine instructions ultimately produced by the compiler. Currently, the `dev` backend tends to overuse memory and the stack, when ultimatly, intermediate computations could be stored in registers and not moved into memory. 
For example, the instructions:
```elm
x = 5
y = 3
z = x + y
z = 2 * z
```
might be translated into 
```asm
# .text
mov $5 X_LOC # x = 5
mov $3 Y_LOC # y = 3
mov [X_LOC] %rax # %0 = x
mov [Y_LOC] %rbx # %1 = y
add %rbx %rax # %0 = %0 + %1
mov %rax Z_LOC # Z = %0
mov [Z_LOC] %rax # %0 = Z
mul $2 %rax # %0 = %0 * 2
mov %rax Z_LOC # Z = %0
# ...
# .data
X_LOC: .byte
Y_LOC: .byte
Z_LOC: .byte
```
(Unless otherwise specified, AT&T arguement order is used (destination **last**), and GAS syntax used)
(This example is extremely prelimanry, I didn't check that this is what is actually generated, or even that this is valid assembly)
To instead
```asm
# .text
mov $5 X_LOC # x = 5
mov $3 Y_LOC # y = 3
mov [X_LOC] %rax # %0 = x
mov [Y_LOC] %rbx # %1 = y
add %rbx %rax # %0 = %0 + %1
mul $2 %rax # %0 = %0 * 2
mov %rax Z_LOC # Z = %0
# ...
# .data
X_LOC: .byte
Y_LOC: .byte
Z_LOC: .byte
```
## Outline

### ROAR Goals

## ROAR Storage and Values

ROAR instructions consist of three parts, the operation itself, `add`, `sub`, `mov`, among many others, the source operand(s), that is the arguements passed to the function, and the destination operand, where the value is being stored. 
In this manual, if a operation is in the form `Op S [...] D`, then `Op` is the operand, `S` (and the rest written `...`) is the source, and `D` is the result operand, so, for instance, `add %0 %1 %2` is saying set registers 2 to the sum of registers 1 and 0, there are some operations that don't have source and/or destination operand(s), however these will be explictly noted 

### Abstract Registers 
The most important feature of ROAR is the Abstract Register, which are used to:
- Store intermediate values
- Act as variables 
- Act as refrences 
- Pass arguements

Here, they are notated as a percent sign (`%`) followed by some number, corresponding to the register number. As opposed to computer registers, there are an infinite[^1] amount, so, for instance, `%432432` is a register, albiet one that will probably not be used
One very importatnt distinction between computer and ROAR registers, *ROAR registers represent presistent data*. This means, that, for example, even if there are 400 operations in between `x=4` and `x`'s next usage, `x` will still be stored in a register. Why the change? To make it easier to optimize code, because a distinction is not drawn between registers and small pieces of memory, we can decide whether to load something into memory or keep it in the computer registers by what is more efficient 
#### Types
To put it simply, Abstract Registers are completly untyped, 64 bit wide storage spaces. That is to say, there is nothing in a register itself to say whether the bits (written in hex for brevity) `00 00 00 00 00 00 00 3F` represent the charecter `?`, the unsigned number 63, or the signed number +63. It is entierly the job of operations to say whether they are working on signed or unsigned arguements (with unsigned being assumed herein)

### Abstract Stack 
While most operations only change information in the destination operand, there are two large exceptions, `jmp` and `call`[^2]. In ROAR, it is entierally up to the caller to save registers that will be operated on, that is to say, if `%3` is some value before a call, you should not expect it to be the same value after the call!
To solve this, one should use the *abstract stack* to "save" registers that one wants to persist across changes in code location
This is the only use of the abstract stack in ROAR, however. Call sites and returns are abstracted away in the instructions of `call` and `ret`, and arguements should be passed in registers, not on the stack

### Structure Registers
There is one final thing to talk about, and that is structured memory. That is to say, representing information that contains a number of bytes. While it is technically possible to do this with just a large amount of registers, it would mean that for each time that we want to preserve the value across a call, it would take $2* \frac{Bytes}{8}$ instruction for each such call.
To solve this, we use "structure registers". The reason that this is in quotes is for a number of reasons. Firstly, these are not at all like regular registers. For one, they need to be created before they can be used, and they can't be inputs *or* outputs to operations.
So then how are these useful? Well, to create a structure of size of let's say 256 bytes (2048 bits), we write `create $2048 %0`. What this instruction does is allocates 800 bits of memory, and then stores a "refrence" to that in register 0.
Why this choice? Because it is very unlikely that we will be storing all 256 bytes (indeed, we would have to be using 16 registers to do so) we don't need to be able to talk about each of the bytes individually, so instead we just treat it like a region of memory, which most likely because of the inefficency of moving a large number of bytes will most likely *speed up* rather then slow down our resulting machine code

> [!IMPORTANT]
> Maybe also add a `LEA` instruction so we can do efficent indexing and passing to registers

Then, when we want to read, say, byte 97 of this structure into register 2, we write `mov [%0+$97] %2`, which is just indirect adressing[^3]. In the abstract machine, it is invalid to take something that was not given as a structure refrence and try and get its value
Note that `mov %0 %1`, `mov [%0] %1`, `mov %0 [%1]` and `mov [%0] [%1]` all mean diffrent things (assuming that both registers 0 and 1 are structure refrences)
- `mov %0 %1` means "make register 1 a refrence to whatever register zero is a refrence to"
- `mov [%0] %1` means "set register 1 to the first 8 bytes of the structure which is refered to by register 0
- `mov %0 [%1]` means "set the first 8 bytes in the structure refered to by register 1 to the refrence to the structure refered to by structure 0"
- `mov [%0] [%1]` means "set the first 8 bytes of the structure refered to by reigster 1 to the first 8 bytes of the structure refered to by register 0

# TODO (This document)
- [ ] : Talk about calls and function blocks
- [ ] : Talk about IO
- [ ] : Talk about jumps and flags
- [ ] : Probaly some other stuff I forgot


[^1]: Not, in practice, actually infinite, instead represented by a integer, mostly likely an unsigned 32 bit integer. However, this *is* more than 4 billion registers, which should be more than enough 
[^2]: Technincally `jmp` and `call` themselves don't alter the registers, but they *do* cause them to be altered
[^3]: Because one major feature of ROAR is all things being assumed to be 64 bits, each of these *must* be aligned to the 8 byte mark, so the above is actually equivalent to `mov [%0+$97*8] %2`