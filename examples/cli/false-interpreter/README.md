# False Interpreter

This is an interpreter for the [false programming language](https://strlen.com/false-language/).
It is currently functional but runs in a way that devours stack space.
There are many examples of applications in the examples sub folder.
Many of them will currently cause stack overflows if stack size is not increased with something like `ulimit -s unlimited`.
