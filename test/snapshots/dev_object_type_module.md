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
x64mac=81f7f653671613525582f81cdd708314e507cb9ff9f473ec40c8578376479852
x64win=ad1650cf281a87d60d07117e2cc42b17d512412a69fbcc2fed3b7ece6852c0e9
x64freebsd=abece5a8da2eff34e26173407d0b95fbbb9deaeef1e5d070a99711e22d4c7422
x64openbsd=b9ad2cf8c1158f96511a471ae04f929835a910aa5ff98cec0538f96e82950c72
x64netbsd=2a151f45c19082462aa9117d0c19956a8028318602f446a30f1d185ccf83a716
x64musl=2a151f45c19082462aa9117d0c19956a8028318602f446a30f1d185ccf83a716
x64glibc=2a151f45c19082462aa9117d0c19956a8028318602f446a30f1d185ccf83a716
x64linux=2a151f45c19082462aa9117d0c19956a8028318602f446a30f1d185ccf83a716
x64elf=2a151f45c19082462aa9117d0c19956a8028318602f446a30f1d185ccf83a716
x64v1mac=81f7f653671613525582f81cdd708314e507cb9ff9f473ec40c8578376479852
x64v1win=ad1650cf281a87d60d07117e2cc42b17d512412a69fbcc2fed3b7ece6852c0e9
x64v1freebsd=2a151f45c19082462aa9117d0c19956a8028318602f446a30f1d185ccf83a716
x64v1openbsd=2a151f45c19082462aa9117d0c19956a8028318602f446a30f1d185ccf83a716
x64v1netbsd=2a151f45c19082462aa9117d0c19956a8028318602f446a30f1d185ccf83a716
x64v1musl=2a151f45c19082462aa9117d0c19956a8028318602f446a30f1d185ccf83a716
x64v1glibc=2a151f45c19082462aa9117d0c19956a8028318602f446a30f1d185ccf83a716
x64v1linux=2a151f45c19082462aa9117d0c19956a8028318602f446a30f1d185ccf83a716
x64v1elf=2a151f45c19082462aa9117d0c19956a8028318602f446a30f1d185ccf83a716
arm64mac=30ab958b06ff3cad41f39f07ab5bb789a3c2c1f12ac10978d64a666f94e0f3cf
arm64win=9fe5192b3d873234c5a6e0a27a66a3769bafcaaeac1cdd6df2c355639a5de0b6
arm64linux=3a2f2903ba6ca687763e1d4bd8cc47c2c373b9ae00c51f4c7bd4a642840b37fd
arm64musl=3a2f2903ba6ca687763e1d4bd8cc47c2c373b9ae00c51f4c7bd4a642840b37fd
arm64glibc=3a2f2903ba6ca687763e1d4bd8cc47c2c373b9ae00c51f4c7bd4a642840b37fd
arm64v1win=9fe5192b3d873234c5a6e0a27a66a3769bafcaaeac1cdd6df2c355639a5de0b6
arm64v1linux=3a2f2903ba6ca687763e1d4bd8cc47c2c373b9ae00c51f4c7bd4a642840b37fd
arm64v1musl=3a2f2903ba6ca687763e1d4bd8cc47c2c373b9ae00c51f4c7bd4a642840b37fd
arm64v1glibc=3a2f2903ba6ca687763e1d4bd8cc47c2c373b9ae00c51f4c7bd4a642840b37fd
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
