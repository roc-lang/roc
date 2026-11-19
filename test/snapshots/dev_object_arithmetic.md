# META
~~~ini
description=Integer arithmetic with I64 return type
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

main : I64
main = add(3, 4) * 2

add : I64, I64 -> I64
add = |a, b| a + b
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : I64 }
    exposes []
    packages {}
    provides { "roc_main": main_for_host }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
    }

main_for_host : I64
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# app
main = add(3, 4) * 2
add = |a, b| a + b

~~~
# DEV OUTPUT
~~~ini
x64mac=0791a23adc3f8fc3a79fa639891816b83210e0bb25fcafe1c933b199517a7854
x64win=4a3b79ea7760fc2cbf1a058b9dee3c2f8294dc9ce1e7f076b498734dd4cf35c4
x64mingw=4a3b79ea7760fc2cbf1a058b9dee3c2f8294dc9ce1e7f076b498734dd4cf35c4
x64freebsd=0c2e0a40ea5ca14f7b18a8aea17138f80b5884fcce69ee3a109c07b15f2205a5
x64openbsd=d34fc22088f061c1e32cb6334af339d896a78fb7d500f207a8676ca0dd73c1a8
x64netbsd=bf34534b0c3c9b05e78b7cb04034ca334ad1d8774ee5a68d02347136f36b8285
x64musl=bf34534b0c3c9b05e78b7cb04034ca334ad1d8774ee5a68d02347136f36b8285
x64glibc=bf34534b0c3c9b05e78b7cb04034ca334ad1d8774ee5a68d02347136f36b8285
x64linux=bf34534b0c3c9b05e78b7cb04034ca334ad1d8774ee5a68d02347136f36b8285
x64elf=bf34534b0c3c9b05e78b7cb04034ca334ad1d8774ee5a68d02347136f36b8285
x64v1mac=0791a23adc3f8fc3a79fa639891816b83210e0bb25fcafe1c933b199517a7854
x64v1win=4a3b79ea7760fc2cbf1a058b9dee3c2f8294dc9ce1e7f076b498734dd4cf35c4
x64v1mingw=4a3b79ea7760fc2cbf1a058b9dee3c2f8294dc9ce1e7f076b498734dd4cf35c4
x64v1freebsd=0c2e0a40ea5ca14f7b18a8aea17138f80b5884fcce69ee3a109c07b15f2205a5
x64v1openbsd=d34fc22088f061c1e32cb6334af339d896a78fb7d500f207a8676ca0dd73c1a8
x64v1netbsd=bf34534b0c3c9b05e78b7cb04034ca334ad1d8774ee5a68d02347136f36b8285
x64v1musl=bf34534b0c3c9b05e78b7cb04034ca334ad1d8774ee5a68d02347136f36b8285
x64v1glibc=bf34534b0c3c9b05e78b7cb04034ca334ad1d8774ee5a68d02347136f36b8285
x64v1linux=bf34534b0c3c9b05e78b7cb04034ca334ad1d8774ee5a68d02347136f36b8285
x64v1elf=bf34534b0c3c9b05e78b7cb04034ca334ad1d8774ee5a68d02347136f36b8285
arm64mac=8a55f63a2dd6367a1129d9e23a93271c2c1720e3beb255c0f1c8e6886fe397e7
arm64win=a45004cd92f039ccec2c923fc69d4daef489cd4a9c31afe0302198541213a603
arm64mingw=a45004cd92f039ccec2c923fc69d4daef489cd4a9c31afe0302198541213a603
arm64linux=84df57f097cb7d0e711ada8e660d7f884c7be17d669c8bde8ceac64df0787b5e
arm64musl=84df57f097cb7d0e711ada8e660d7f884c7be17d669c8bde8ceac64df0787b5e
arm64glibc=84df57f097cb7d0e711ada8e660d7f884c7be17d669c8bde8ceac64df0787b5e
arm64v1win=a45004cd92f039ccec2c923fc69d4daef489cd4a9c31afe0302198541213a603
arm64v1mingw=a45004cd92f039ccec2c923fc69d4daef489cd4a9c31afe0302198541213a603
arm64v1linux=84df57f097cb7d0e711ada8e660d7f884c7be17d669c8bde8ceac64df0787b5e
arm64v1musl=84df57f097cb7d0e711ada8e660d7f884c7be17d669c8bde8ceac64df0787b5e
arm64v1glibc=84df57f097cb7d0e711ada8e660d7f884c7be17d669c8bde8ceac64df0787b5e
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
