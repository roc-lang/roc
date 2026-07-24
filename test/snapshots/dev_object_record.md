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
x64mac=c28691a9744de11848703ee85558841661618e18dd3ec1f6fd6db9aaf05de754
x64win=2128647a5d46b7628908cfe96a8e6e1c593f35de14d1ac68a408e9dd5cfa4b90
x64freebsd=bf9c9bd2bfc45ca1045f897986061cf68804f2647fd58377cbbbb0ecd8c118d6
x64openbsd=bf9c9bd2bfc45ca1045f897986061cf68804f2647fd58377cbbbb0ecd8c118d6
x64netbsd=bf9c9bd2bfc45ca1045f897986061cf68804f2647fd58377cbbbb0ecd8c118d6
x64musl=bf9c9bd2bfc45ca1045f897986061cf68804f2647fd58377cbbbb0ecd8c118d6
x64glibc=bf9c9bd2bfc45ca1045f897986061cf68804f2647fd58377cbbbb0ecd8c118d6
x64linux=bf9c9bd2bfc45ca1045f897986061cf68804f2647fd58377cbbbb0ecd8c118d6
x64elf=bf9c9bd2bfc45ca1045f897986061cf68804f2647fd58377cbbbb0ecd8c118d6
arm64mac=4f65c785378f2b9f8b1a8da66cf57bf41db5d4a2d5844b88d2f0941ebdfb5922
arm64win=87c08f5fc5a3e80c56c5efe85e07e44ab690395ca28c2a55db4f0addae27bcfc
arm64linux=783fdd3eee3c4d62fa105eff08d96afda717637a8468974f907d556f0c11c874
arm64musl=783fdd3eee3c4d62fa105eff08d96afda717637a8468974f907d556f0c11c874
arm64glibc=783fdd3eee3c4d62fa105eff08d96afda717637a8468974f907d556f0c11c874
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
