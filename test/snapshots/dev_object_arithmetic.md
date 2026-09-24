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
x64mac=0706cd1ed65c6748039179a5be054f1b5b82f98ab3640f13778e84891efb431b
x64win=6adac657dc8c9c5b7be068042dda2fd8eb3e1b31ae3a79affb7b2d5acecec911
x64mingw=6adac657dc8c9c5b7be068042dda2fd8eb3e1b31ae3a79affb7b2d5acecec911
x64freebsd=35a52471bbc9c7506634bc8b4997875cd0061ea0c0fac7611d868e1a25473216
x64openbsd=9dca03d7990747e714b7969d2b232c72a273cdb610d1a9f2465abab9ec008160
x64netbsd=c55885fd08c69d0bd989faccf3ca70f177373aae54b46f32688ebe1d598fc460
x64musl=c55885fd08c69d0bd989faccf3ca70f177373aae54b46f32688ebe1d598fc460
x64glibc=c55885fd08c69d0bd989faccf3ca70f177373aae54b46f32688ebe1d598fc460
x64linux=c55885fd08c69d0bd989faccf3ca70f177373aae54b46f32688ebe1d598fc460
x64elf=c55885fd08c69d0bd989faccf3ca70f177373aae54b46f32688ebe1d598fc460
x64v1mac=0706cd1ed65c6748039179a5be054f1b5b82f98ab3640f13778e84891efb431b
x64v1win=6adac657dc8c9c5b7be068042dda2fd8eb3e1b31ae3a79affb7b2d5acecec911
x64v1mingw=6adac657dc8c9c5b7be068042dda2fd8eb3e1b31ae3a79affb7b2d5acecec911
x64v1freebsd=35a52471bbc9c7506634bc8b4997875cd0061ea0c0fac7611d868e1a25473216
x64v1openbsd=9dca03d7990747e714b7969d2b232c72a273cdb610d1a9f2465abab9ec008160
x64v1netbsd=c55885fd08c69d0bd989faccf3ca70f177373aae54b46f32688ebe1d598fc460
x64v1musl=c55885fd08c69d0bd989faccf3ca70f177373aae54b46f32688ebe1d598fc460
x64v1glibc=c55885fd08c69d0bd989faccf3ca70f177373aae54b46f32688ebe1d598fc460
x64v1linux=c55885fd08c69d0bd989faccf3ca70f177373aae54b46f32688ebe1d598fc460
x64v1elf=c55885fd08c69d0bd989faccf3ca70f177373aae54b46f32688ebe1d598fc460
arm64mac=511e75deaf0538d1a80e1161b686d8b6c3b24b1022a01d005dfc37e6ff42763d
arm64win=ab319cad39d657d69e3f7fec5f16baa086d771f7046ee6904fa53d6f425c8934
arm64mingw=ab319cad39d657d69e3f7fec5f16baa086d771f7046ee6904fa53d6f425c8934
arm64linux=11278617e5f11f0da3600bc157048b761903399e766e8e4999ba0e0798d3d556
arm64musl=11278617e5f11f0da3600bc157048b761903399e766e8e4999ba0e0798d3d556
arm64glibc=11278617e5f11f0da3600bc157048b761903399e766e8e4999ba0e0798d3d556
arm64v1win=ab319cad39d657d69e3f7fec5f16baa086d771f7046ee6904fa53d6f425c8934
arm64v1mingw=ab319cad39d657d69e3f7fec5f16baa086d771f7046ee6904fa53d6f425c8934
arm64v1linux=11278617e5f11f0da3600bc157048b761903399e766e8e4999ba0e0798d3d556
arm64v1musl=11278617e5f11f0da3600bc157048b761903399e766e8e4999ba0e0798d3d556
arm64v1glibc=11278617e5f11f0da3600bc157048b761903399e766e8e4999ba0e0798d3d556
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
