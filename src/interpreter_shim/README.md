# interpreter shim

- the interpreter shim is compiled to an object file and linked with a roc platform host to create a "interpreter" host for the platform. This executable is cached, and is then used by the roc cli for fast execution of roc programs in a development loop.
- the shim provides the functionality to execute roc programs using the roc interpreter
- the interpreter host executable is spawned as a child of the roc cli, and communicates via shared memory for inter-process communication.
