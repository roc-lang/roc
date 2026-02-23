# META
~~~ini
description=Type module import with multi-module compilation
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

import Color

main = Color.to_str(Color.red)
~~~
## Color.roc
~~~roc
module [Color, red, green, blue, to_str]

Color : [Red, Green, Blue]

red : Color
red = Red

green : Color
green = Green

blue : Color
blue = Blue

to_str : Color -> Str
to_str = |color|
    match color {
        Red => "red"
        Green => "green"
        Blue => "blue"
    }
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : Str }
    exposes []
    packages {}
    provides { main_for_host: "main" }
    targets: {
        files: "targets/",
        exe: {
            x64glibc: [app],
        }
    }

main_for_host : Str
main_for_host = main
~~~
# MONO
~~~roc
# app
main = to_str(red)

# Color
red = 2
green = True
blue = False
to_str = |color| match color {
	Red => "red"
	Green => "green"
	Blue => "blue"
}

# platform
main_for_host = <required>

~~~
# DEV OUTPUT
~~~ini
x64mac=0cea1204f7042bf205b56e73e1dc55cd00ae84aa28c379980a2d90d5600e4845
x64win=5cd5c3660e350fe70e498f5e4fe98fec291c046421c7f4be739830bb6d9c3239
x64freebsd=45d63da8527797f3a036e966b36a944e475e82f72313d5bb7934b8a7804636b0
x64openbsd=45d63da8527797f3a036e966b36a944e475e82f72313d5bb7934b8a7804636b0
x64netbsd=45d63da8527797f3a036e966b36a944e475e82f72313d5bb7934b8a7804636b0
x64musl=45d63da8527797f3a036e966b36a944e475e82f72313d5bb7934b8a7804636b0
x64glibc=45d63da8527797f3a036e966b36a944e475e82f72313d5bb7934b8a7804636b0
x64linux=45d63da8527797f3a036e966b36a944e475e82f72313d5bb7934b8a7804636b0
x64elf=45d63da8527797f3a036e966b36a944e475e82f72313d5bb7934b8a7804636b0
arm64mac=7c191aa1123c3e6e669e5a339bc11ea3dcea8f4baf22f5a4b143bfd510c98f49
arm64win=0c381fd4aec6853aa7a7b7f8a095e9d9396bf03a3d347be0e48f44086785f611
arm64linux=5e8584aea96fe3e137d9017469ed554c62fcecf0921a5eeaadafe023e85be0b0
arm64musl=5e8584aea96fe3e137d9017469ed554c62fcecf0921a5eeaadafe023e85be0b0
arm64glibc=5e8584aea96fe3e137d9017469ed554c62fcecf0921a5eeaadafe023e85be0b0
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
