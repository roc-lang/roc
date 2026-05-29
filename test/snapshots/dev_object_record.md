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
# platform
name_for_host = <required>
score_for_host = <required>

# app
name = "Alice"
score = 42

~~~
# DEV OUTPUT
~~~ini
x64mac=8a0973f34b7989b6830b0aef188855324d3457e0c0a21b3db912507fa7bd96b1
x64win=4dfd3f3d9b75177e286b920d773d62d0ed4b878472313124ea258a3513900c27
x64freebsd=f358795d9a59b46915d9f53f1a348ca5fdf16af5420e5822b62a2a3e7335285f
x64openbsd=f358795d9a59b46915d9f53f1a348ca5fdf16af5420e5822b62a2a3e7335285f
x64netbsd=f358795d9a59b46915d9f53f1a348ca5fdf16af5420e5822b62a2a3e7335285f
x64musl=f358795d9a59b46915d9f53f1a348ca5fdf16af5420e5822b62a2a3e7335285f
x64glibc=f358795d9a59b46915d9f53f1a348ca5fdf16af5420e5822b62a2a3e7335285f
x64linux=f358795d9a59b46915d9f53f1a348ca5fdf16af5420e5822b62a2a3e7335285f
x64elf=f358795d9a59b46915d9f53f1a348ca5fdf16af5420e5822b62a2a3e7335285f
arm64mac=c164e5be92fe5a50a6e7ea78954edec59d3ca9fdd572b968ff884ec47a80dac0
arm64win=98046472bc39291938343318717cdecc440e0eca7a58e896b244b28f62f6b1d8
arm64linux=b86884e145663e8d0fadbb2bf9653aa816f166664ee29277ce3c5fcecf82e376
arm64musl=b86884e145663e8d0fadbb2bf9653aa816f166664ee29277ce3c5fcecf82e376
arm64glibc=b86884e145663e8d0fadbb2bf9653aa816f166664ee29277ce3c5fcecf82e376
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
