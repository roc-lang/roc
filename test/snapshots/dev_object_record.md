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
arm64mac=2998adde7c8453e165ebf29c63dbf0f605a3c8d5b70fd96577020a6358e4b6c7
arm64win=33e3fe3b68f512c48d8ad5cb7f1152b0f8278541eb627a774f6ade98aa814161
arm64linux=7d8fabbd2ac4ed2df4d2767aced91a8afd907dd2c42338e8c7e4ceb24e8a9d7a
arm64musl=7d8fabbd2ac4ed2df4d2767aced91a8afd907dd2c42338e8c7e4ceb24e8a9d7a
arm64glibc=7d8fabbd2ac4ed2df4d2767aced91a8afd907dd2c42338e8c7e4ceb24e8a9d7a
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
