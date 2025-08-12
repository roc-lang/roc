# Interpreter Shim

The interpreter shim is a key component for providing a fast development experience with the `roc` CLI.

It is compiled into an object file and linked with a Roc platform host to create an interpreter-host. This interpreter-host executable is cached and then used by the `roc` CLI to run Roc programs quickly during development.

The shim enables the execution of Roc programs via the interpreter. The interpreter-host is spawned as a child process of the `roc` CLI, and it communicates with the CLI using shared memory for efficient inter-process communication.
