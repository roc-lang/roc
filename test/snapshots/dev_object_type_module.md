# META
~~~ini
description=Type mod import with multi-mod compilation
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

import Color

main = Color.to_str(Color.red({}))
~~~
## Color.roc
~~~roc
Color := [Red, Green, Blue].{
    red : {} -> Color
    red = |{}| Red

    green : {} -> Color
    green = |{}| Green

    blue : {} -> Color
    blue = |{}| Blue

    to_str : Color -> Str
    to_str = |color|
        match color {
            Red => "red"
            Green => "green"
            Blue => "blue"
        }
}
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

# Color
Color.red = |{}| Red
Color.green = |{}| Green
Color.blue = |{}| Blue
Color.to_str = |color| match color {
	Red => "red"
	Green => "green"
	Blue => "blue"
}

# app
main = to_str(red({}))

~~~
# DEV OUTPUT
~~~ini
x64mac=043ebca60b56eb788533ab33a9b02ed7e85c86dfa6307cd3537e4676c21919a4
x64win=4fb54824b4a3f4a1f132830e81f1388a2eadf93126fa21c230619236e1d2ede5
x64mingw=4fb54824b4a3f4a1f132830e81f1388a2eadf93126fa21c230619236e1d2ede5
x64freebsd=1bcf64996c3ce1d0ab6314473c4a454ac15e63eab32a81c6ebba9643e494efa5
x64openbsd=971694ebf3ad41680fcde650931258116fa50fcd5ac0a9f7a9e47b547267a3cd
x64netbsd=a50a8bf4de49ed192000ae81ac4207d2ccc4504a8481f2a4df2bf86f5d5e9cc1
x64musl=a50a8bf4de49ed192000ae81ac4207d2ccc4504a8481f2a4df2bf86f5d5e9cc1
x64glibc=a50a8bf4de49ed192000ae81ac4207d2ccc4504a8481f2a4df2bf86f5d5e9cc1
x64linux=a50a8bf4de49ed192000ae81ac4207d2ccc4504a8481f2a4df2bf86f5d5e9cc1
x64elf=a50a8bf4de49ed192000ae81ac4207d2ccc4504a8481f2a4df2bf86f5d5e9cc1
x64v1mac=043ebca60b56eb788533ab33a9b02ed7e85c86dfa6307cd3537e4676c21919a4
x64v1win=4fb54824b4a3f4a1f132830e81f1388a2eadf93126fa21c230619236e1d2ede5
x64v1mingw=4fb54824b4a3f4a1f132830e81f1388a2eadf93126fa21c230619236e1d2ede5
x64v1freebsd=1bcf64996c3ce1d0ab6314473c4a454ac15e63eab32a81c6ebba9643e494efa5
x64v1openbsd=971694ebf3ad41680fcde650931258116fa50fcd5ac0a9f7a9e47b547267a3cd
x64v1netbsd=a50a8bf4de49ed192000ae81ac4207d2ccc4504a8481f2a4df2bf86f5d5e9cc1
x64v1musl=a50a8bf4de49ed192000ae81ac4207d2ccc4504a8481f2a4df2bf86f5d5e9cc1
x64v1glibc=a50a8bf4de49ed192000ae81ac4207d2ccc4504a8481f2a4df2bf86f5d5e9cc1
x64v1linux=a50a8bf4de49ed192000ae81ac4207d2ccc4504a8481f2a4df2bf86f5d5e9cc1
x64v1elf=a50a8bf4de49ed192000ae81ac4207d2ccc4504a8481f2a4df2bf86f5d5e9cc1
arm64mac=08de4168e32f3fe9554ae5278cf9e92a63f3f552c110eeaaf2b197ba2abd772b
arm64win=e580c82ad9bbcf0234b0ae23f841d462521b167e751410ad02b512b5be0bb0c2
arm64mingw=e580c82ad9bbcf0234b0ae23f841d462521b167e751410ad02b512b5be0bb0c2
arm64linux=2a58656ddd9590719f3df87bf8063f82478f8775ecfa73894e49a48fe68af8a3
arm64musl=2a58656ddd9590719f3df87bf8063f82478f8775ecfa73894e49a48fe68af8a3
arm64glibc=2a58656ddd9590719f3df87bf8063f82478f8775ecfa73894e49a48fe68af8a3
arm64v1win=e580c82ad9bbcf0234b0ae23f841d462521b167e751410ad02b512b5be0bb0c2
arm64v1mingw=e580c82ad9bbcf0234b0ae23f841d462521b167e751410ad02b512b5be0bb0c2
arm64v1linux=2a58656ddd9590719f3df87bf8063f82478f8775ecfa73894e49a48fe68af8a3
arm64v1musl=2a58656ddd9590719f3df87bf8063f82478f8775ecfa73894e49a48fe68af8a3
arm64v1glibc=2a58656ddd9590719f3df87bf8063f82478f8775ecfa73894e49a48fe68af8a3
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
