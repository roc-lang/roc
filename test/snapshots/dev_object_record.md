# META
~~~ini
description=Multiple provides entries with two entrypoints
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [name, score] { pf: platform "./platform.roc" }

name = "Alice"

score : I64
score = 42
~~~
## platform.roc
~~~roc
platform ""
    requires {} { name : Str, score : I64 }
    exposes []
    packages {}
    provides { "roc_name": name_for_host, "roc_score": score_for_host }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
    }

name_for_host : Str
name_for_host = name

score_for_host : I64
score_for_host = score
~~~
# MONO
~~~roc
# platform
name_for_host = <required>
score_for_host = <required>

# app
name = "Alice"
score = 42

~~~
# DEV OUTPUT
~~~ini
x64mac=942f612f7a445a78a38ba4fae0e81d61829416fe6229de1643ef0cc41a9ac26e
x64win=e30ad5a97e1e6124552a0ab34085924cbf5375ea20c68a4fb357f15cad96e3ee
x64freebsd=d2b7c7b537631a72f3d0b9882537365c9bb0aad869c7d42d196a107b8f35049f
x64openbsd=d2b7c7b537631a72f3d0b9882537365c9bb0aad869c7d42d196a107b8f35049f
x64netbsd=d2b7c7b537631a72f3d0b9882537365c9bb0aad869c7d42d196a107b8f35049f
x64musl=d2b7c7b537631a72f3d0b9882537365c9bb0aad869c7d42d196a107b8f35049f
x64glibc=d2b7c7b537631a72f3d0b9882537365c9bb0aad869c7d42d196a107b8f35049f
x64linux=d2b7c7b537631a72f3d0b9882537365c9bb0aad869c7d42d196a107b8f35049f
x64elf=d2b7c7b537631a72f3d0b9882537365c9bb0aad869c7d42d196a107b8f35049f
arm64mac=df6d78104e5c295518c04485d70a905b5883cc35207daad29eb50754aa838094
arm64win=03471e38e5d89948f0fa26131cc98b2a2fd963bc9fb7127a5a6e6a8c46142889
arm64linux=170b9535b53dd797cba9480e8c0f938ef74719b0ff3fbb4ab5d4299857146524
arm64musl=170b9535b53dd797cba9480e8c0f938ef74719b0ff3fbb4ab5d4299857146524
arm64glibc=170b9535b53dd797cba9480e8c0f938ef74719b0ff3fbb4ab5d4299857146524
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
