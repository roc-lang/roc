# META
~~~ini
description=Hello world dev object compilation
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

main = "Hello, World!"
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : Str }
    exposes []
    packages {}
    provides { "roc_main": main_for_host }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
    }

main_for_host : Str
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# app
main = "Hello, World!"

~~~
# DEV OUTPUT
~~~ini
x64mac=137610473c660bbfb826afb1f58ac46027f260d79ea1fcf40250ff3f395e76fc
x64win=acac8525063a17b607674d81cbef6e81425506e6406dfcead6636341a6054b7b
x64freebsd=368b435c9339c5bee747df8415568286251a11bbcc3d3f284568f44321827e15
x64openbsd=368b435c9339c5bee747df8415568286251a11bbcc3d3f284568f44321827e15
x64netbsd=368b435c9339c5bee747df8415568286251a11bbcc3d3f284568f44321827e15
x64musl=368b435c9339c5bee747df8415568286251a11bbcc3d3f284568f44321827e15
x64glibc=368b435c9339c5bee747df8415568286251a11bbcc3d3f284568f44321827e15
x64linux=368b435c9339c5bee747df8415568286251a11bbcc3d3f284568f44321827e15
x64elf=368b435c9339c5bee747df8415568286251a11bbcc3d3f284568f44321827e15
arm64mac=e469209167738d1e2d9605b0e20851fa503c6c1a6fc1d7508d804a044bfe7e4a
arm64win=4c2af047ef9db4c386bd4747c05082a91beb4dde12a256cce7399d976d4297a2
arm64linux=87d1a8488bc40441985dd4c7fc7c0c297a7d63e75527c9fa6238443f50f7d7b3
arm64musl=87d1a8488bc40441985dd4c7fc7c0c297a7d63e75527c9fa6238443f50f7d7b3
arm64glibc=87d1a8488bc40441985dd4c7fc7c0c297a7d63e75527c9fa6238443f50f7d7b3
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
