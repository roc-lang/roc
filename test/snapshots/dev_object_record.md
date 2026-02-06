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
x64mac=f084c5e0b5e35b17a07bcf1c13521736c9f397654c44d8c65dcfa2c8a91c1a1c
x64win=ae9931a8ba1569e1df2af55c041d7af83184402d9fadff9aa6c9eb5007a51fc4
x64freebsd=0fb1e7cac1f72f0c33d6bb225529a1cedb7705079a637e14bfd4802bc70a434c
x64openbsd=0fb1e7cac1f72f0c33d6bb225529a1cedb7705079a637e14bfd4802bc70a434c
x64netbsd=0fb1e7cac1f72f0c33d6bb225529a1cedb7705079a637e14bfd4802bc70a434c
x64musl=0fb1e7cac1f72f0c33d6bb225529a1cedb7705079a637e14bfd4802bc70a434c
x64glibc=0fb1e7cac1f72f0c33d6bb225529a1cedb7705079a637e14bfd4802bc70a434c
x64linux=0fb1e7cac1f72f0c33d6bb225529a1cedb7705079a637e14bfd4802bc70a434c
x64elf=0fb1e7cac1f72f0c33d6bb225529a1cedb7705079a637e14bfd4802bc70a434c
arm64mac=74aed79b743325d00498e646de3d63bb3a641dea0fe513ae74ea0dd9328a60e4
arm64win=6de15d52b5cde87ead6f66a71a4f58b05c49bb7fb10758f08edaecf38e7b44f2
arm64linux=257d01069a3bedb66fedb9a53725cc3f49c62e9bc780992cb1361223d9625b0c
arm64musl=257d01069a3bedb66fedb9a53725cc3f49c62e9bc780992cb1361223d9625b0c
arm64glibc=257d01069a3bedb66fedb9a53725cc3f49c62e9bc780992cb1361223d9625b0c
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
