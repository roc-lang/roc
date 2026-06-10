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
        inputs: "targets/",
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
x64mac=607bec42f595c605b19d8e7423eae0fe827c315f5c9b36d78edf8f66c7b1d99d
x64win=4dfd3f3d9b75177e286b920d773d62d0ed4b878472313124ea258a3513900c27
x64freebsd=f358795d9a59b46915d9f53f1a348ca5fdf16af5420e5822b62a2a3e7335285f
x64openbsd=f358795d9a59b46915d9f53f1a348ca5fdf16af5420e5822b62a2a3e7335285f
x64netbsd=f358795d9a59b46915d9f53f1a348ca5fdf16af5420e5822b62a2a3e7335285f
x64musl=f358795d9a59b46915d9f53f1a348ca5fdf16af5420e5822b62a2a3e7335285f
x64glibc=f358795d9a59b46915d9f53f1a348ca5fdf16af5420e5822b62a2a3e7335285f
x64linux=f358795d9a59b46915d9f53f1a348ca5fdf16af5420e5822b62a2a3e7335285f
x64elf=f358795d9a59b46915d9f53f1a348ca5fdf16af5420e5822b62a2a3e7335285f
arm64mac=39d01c214450234567ab6357b4db7895e9390971b542285db7f36bab4dca508b
arm64win=98046472bc39291938343318717cdecc440e0eca7a58e896b244b28f62f6b1d8
arm64linux=b86884e145663e8d0fadbb2bf9653aa816f166664ee29277ce3c5fcecf82e376
arm64musl=b86884e145663e8d0fadbb2bf9653aa816f166664ee29277ce3c5fcecf82e376
arm64glibc=b86884e145663e8d0fadbb2bf9653aa816f166664ee29277ce3c5fcecf82e376
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
