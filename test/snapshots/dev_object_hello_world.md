# META
~~~ini
description=Hello world dev object compilation
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

main = "Hello, World!"
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : Str }
    exposes []
    packages {}
    provides { main_for_host: "main" }
    targets: {
        files: "targets/",
        exe: {
            x64glibc: [app],
        }
    }

main_for_host : Str
main_for_host = main
~~~
# MONO
~~~roc
# app
main = "Hello, World!"

# platform
main_for_host = <required>

~~~
# DEV OUTPUT
~~~ini
x64mac=196a23741f8453b912108fce29894898b09818b695eec840de3b31ec9517ff3b
x64win=dc7412dd32925f8e936c418f644cb2a507e96ddae3cdeb061dd67d47e05ad08c
x64freebsd=b19f155d69c0dae307d78f03692027831b824c2e39e7993add9a6606733ec685
x64openbsd=b19f155d69c0dae307d78f03692027831b824c2e39e7993add9a6606733ec685
x64netbsd=b19f155d69c0dae307d78f03692027831b824c2e39e7993add9a6606733ec685
x64musl=b19f155d69c0dae307d78f03692027831b824c2e39e7993add9a6606733ec685
x64glibc=b19f155d69c0dae307d78f03692027831b824c2e39e7993add9a6606733ec685
x64linux=b19f155d69c0dae307d78f03692027831b824c2e39e7993add9a6606733ec685
x64elf=b19f155d69c0dae307d78f03692027831b824c2e39e7993add9a6606733ec685
arm64mac=355302e7f40504de3cdab36b5c92f370958ca6629f5655cc4642e6594fda6e9a
arm64win=adc6f0883f396b0f59bb4b4c3c4666cd319df94f6a6f0641b18cbc732eb5cc27
arm64linux=958db6aad587af20a1acd916280b3d353df8d8b30d1e95f746b255b3cf7a3ac3
arm64musl=958db6aad587af20a1acd916280b3d353df8d8b30d1e95f746b255b3cf7a3ac3
arm64glibc=958db6aad587af20a1acd916280b3d353df8d8b30d1e95f746b255b3cf7a3ac3
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
