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
x64mac=342de6d7ea788f1becdab0ee73454d1408d3edabdbcdc95a9277c9bc21f5152f
x64win=c307b48833a82a20928e8557b409718614f6cbb8f75916579d4ecf7869a2acaa
x64freebsd=3ac37c935e7c7c816cacbf6378ec0fe6ae26140a5f184a243b4959caf65787f9
x64openbsd=3ac37c935e7c7c816cacbf6378ec0fe6ae26140a5f184a243b4959caf65787f9
x64netbsd=3ac37c935e7c7c816cacbf6378ec0fe6ae26140a5f184a243b4959caf65787f9
x64musl=3ac37c935e7c7c816cacbf6378ec0fe6ae26140a5f184a243b4959caf65787f9
x64glibc=3ac37c935e7c7c816cacbf6378ec0fe6ae26140a5f184a243b4959caf65787f9
x64linux=3ac37c935e7c7c816cacbf6378ec0fe6ae26140a5f184a243b4959caf65787f9
x64elf=3ac37c935e7c7c816cacbf6378ec0fe6ae26140a5f184a243b4959caf65787f9
arm64mac=fe91abf28437d0dce5bf8ba0b79aa0a8aa8f90a0cbf69660b7cf8995eb733192
arm64win=eb2c4646a3c044625208b62da17138c825abc42f6d054a64943264004b5f9812
arm64linux=6ad7389726f43a795beb9369a511829476df132cc856cfb3d2c543e03ec56af7
arm64musl=6ad7389726f43a795beb9369a511829476df132cc856cfb3d2c543e03ec56af7
arm64glibc=6ad7389726f43a795beb9369a511829476df132cc856cfb3d2c543e03ec56af7
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
