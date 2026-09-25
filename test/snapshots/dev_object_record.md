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
x64mac=3483f6de5c3e8d4b17c6e2d67bfe5449de3be582a1b9ecff88d054eb73102d2c
x64win=01a1c0c17171de6c7dcdad870f8fbdc0d7e590fcf45fdd837e23a85322a7c368
x64mingw=01a1c0c17171de6c7dcdad870f8fbdc0d7e590fcf45fdd837e23a85322a7c368
x64freebsd=0d7050afac3ac7a46177f871f2552a3dcba72fe91f3213ba3534c619f7ef46ff
x64openbsd=02fa807ec1bb54cb331bf5c8d4c31982899561844c21004f3008ba23100fdc42
x64netbsd=094a809e47ae12a66df29fe955494cb2765d2a085c6a55fadecd67200cadb5dd
x64musl=094a809e47ae12a66df29fe955494cb2765d2a085c6a55fadecd67200cadb5dd
x64glibc=094a809e47ae12a66df29fe955494cb2765d2a085c6a55fadecd67200cadb5dd
x64linux=094a809e47ae12a66df29fe955494cb2765d2a085c6a55fadecd67200cadb5dd
x64elf=094a809e47ae12a66df29fe955494cb2765d2a085c6a55fadecd67200cadb5dd
x64v1mac=3483f6de5c3e8d4b17c6e2d67bfe5449de3be582a1b9ecff88d054eb73102d2c
x64v1win=01a1c0c17171de6c7dcdad870f8fbdc0d7e590fcf45fdd837e23a85322a7c368
x64v1mingw=01a1c0c17171de6c7dcdad870f8fbdc0d7e590fcf45fdd837e23a85322a7c368
x64v1freebsd=0d7050afac3ac7a46177f871f2552a3dcba72fe91f3213ba3534c619f7ef46ff
x64v1openbsd=02fa807ec1bb54cb331bf5c8d4c31982899561844c21004f3008ba23100fdc42
x64v1netbsd=094a809e47ae12a66df29fe955494cb2765d2a085c6a55fadecd67200cadb5dd
x64v1musl=094a809e47ae12a66df29fe955494cb2765d2a085c6a55fadecd67200cadb5dd
x64v1glibc=094a809e47ae12a66df29fe955494cb2765d2a085c6a55fadecd67200cadb5dd
x64v1linux=094a809e47ae12a66df29fe955494cb2765d2a085c6a55fadecd67200cadb5dd
x64v1elf=094a809e47ae12a66df29fe955494cb2765d2a085c6a55fadecd67200cadb5dd
arm64mac=af832b29734c265848ccbbdfe6e25b5c7fa7226c1632b34a6def594f7240a74f
arm64win=d6758d0720c9828f9d03f82df55b482a3aca1754de03eb07cae80c83f29c6164
arm64mingw=d6758d0720c9828f9d03f82df55b482a3aca1754de03eb07cae80c83f29c6164
arm64linux=3b0add90ba20644498e46ceb36b9e8d7103da6ca85fa33c56cf19ecc298f9cfe
arm64musl=3b0add90ba20644498e46ceb36b9e8d7103da6ca85fa33c56cf19ecc298f9cfe
arm64glibc=3b0add90ba20644498e46ceb36b9e8d7103da6ca85fa33c56cf19ecc298f9cfe
arm64v1win=d6758d0720c9828f9d03f82df55b482a3aca1754de03eb07cae80c83f29c6164
arm64v1mingw=d6758d0720c9828f9d03f82df55b482a3aca1754de03eb07cae80c83f29c6164
arm64v1linux=3b0add90ba20644498e46ceb36b9e8d7103da6ca85fa33c56cf19ecc298f9cfe
arm64v1musl=3b0add90ba20644498e46ceb36b9e8d7103da6ca85fa33c56cf19ecc298f9cfe
arm64v1glibc=3b0add90ba20644498e46ceb36b9e8d7103da6ca85fa33c56cf19ecc298f9cfe
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
