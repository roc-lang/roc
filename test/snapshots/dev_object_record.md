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
    provides { name_for_host: "name", score_for_host: "score" }
    targets: {
        files: "targets/",
        exe: {
            x64glibc: [app],
        }
    }

name_for_host : Str
name_for_host = name

score_for_host : I64
score_for_host = score
~~~
# MONO
~~~roc
# app
name = "Alice"
score = 42

# platform
name_for_host = <required>
score_for_host = <required>

~~~
# DEV OUTPUT
~~~ini
x64mac=8400956d006142c655b6be4cac2414f38544539b7590a93016cf490610abd1e3
x64win=e54f521798029370f2e705a0a1b20377a7bd71751880ac392dd5eed1a19a6b3b
x64freebsd=b545d76813b0daba3f63e7a524a48cc125b2a23fbf5a6fe3b0cc7f74dd86bb98
x64openbsd=b545d76813b0daba3f63e7a524a48cc125b2a23fbf5a6fe3b0cc7f74dd86bb98
x64netbsd=b545d76813b0daba3f63e7a524a48cc125b2a23fbf5a6fe3b0cc7f74dd86bb98
x64musl=b545d76813b0daba3f63e7a524a48cc125b2a23fbf5a6fe3b0cc7f74dd86bb98
x64glibc=b545d76813b0daba3f63e7a524a48cc125b2a23fbf5a6fe3b0cc7f74dd86bb98
x64linux=b545d76813b0daba3f63e7a524a48cc125b2a23fbf5a6fe3b0cc7f74dd86bb98
x64elf=b545d76813b0daba3f63e7a524a48cc125b2a23fbf5a6fe3b0cc7f74dd86bb98
arm64mac=74aed79b743325d00498e646de3d63bb3a641dea0fe513ae74ea0dd9328a60e4
arm64win=6de15d52b5cde87ead6f66a71a4f58b05c49bb7fb10758f08edaecf38e7b44f2
arm64linux=257d01069a3bedb66fedb9a53725cc3f49c62e9bc780992cb1361223d9625b0c
arm64musl=257d01069a3bedb66fedb9a53725cc3f49c62e9bc780992cb1361223d9625b0c
arm64glibc=257d01069a3bedb66fedb9a53725cc3f49c62e9bc780992cb1361223d9625b0c
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
