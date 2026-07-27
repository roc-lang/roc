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
x64mac=c0099a05b47ee4cae0d2e2ac2c3e42cbc7c9536294dd36caeb57b1782431aca3
x64win=2128647a5d46b7628908cfe96a8e6e1c593f35de14d1ac68a408e9dd5cfa4b90
x64freebsd=c0c7287871dcb71db079c8c90bfe3fd7b6e0d5800aa5df426f105af4b75fb235
x64openbsd=c0c7287871dcb71db079c8c90bfe3fd7b6e0d5800aa5df426f105af4b75fb235
x64netbsd=c0c7287871dcb71db079c8c90bfe3fd7b6e0d5800aa5df426f105af4b75fb235
x64musl=c0c7287871dcb71db079c8c90bfe3fd7b6e0d5800aa5df426f105af4b75fb235
x64glibc=c0c7287871dcb71db079c8c90bfe3fd7b6e0d5800aa5df426f105af4b75fb235
x64linux=c0c7287871dcb71db079c8c90bfe3fd7b6e0d5800aa5df426f105af4b75fb235
x64elf=c0c7287871dcb71db079c8c90bfe3fd7b6e0d5800aa5df426f105af4b75fb235
arm64mac=522d58d659e0ae3dbb829d76f0a87350afc9d98aa38a3ca3a7b028185697c149
arm64win=87c08f5fc5a3e80c56c5efe85e07e44ab690395ca28c2a55db4f0addae27bcfc
arm64linux=780dc20cbf6229dbc315ef4f0758dbc64185a1b72b8e12cb056c0634fa5d5574
arm64musl=780dc20cbf6229dbc315ef4f0758dbc64185a1b72b8e12cb056c0634fa5d5574
arm64glibc=780dc20cbf6229dbc315ef4f0758dbc64185a1b72b8e12cb056c0634fa5d5574
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
