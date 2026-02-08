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
x64mac=481f58df5a99f3ca34e46ff26c535052ada887ee8fb4b39760299606f24c31bd
x64win=1cbf8e0593f22cc3decb65fa7f8c545ffe0b9a964f3a8be3335e93ba55af55ba
x64freebsd=b34ec152998b021004c6c7a16132d286d89127b65f3185e22f8ef257cb1ee871
x64openbsd=b34ec152998b021004c6c7a16132d286d89127b65f3185e22f8ef257cb1ee871
x64netbsd=b34ec152998b021004c6c7a16132d286d89127b65f3185e22f8ef257cb1ee871
x64musl=b34ec152998b021004c6c7a16132d286d89127b65f3185e22f8ef257cb1ee871
x64glibc=b34ec152998b021004c6c7a16132d286d89127b65f3185e22f8ef257cb1ee871
x64linux=b34ec152998b021004c6c7a16132d286d89127b65f3185e22f8ef257cb1ee871
x64elf=b34ec152998b021004c6c7a16132d286d89127b65f3185e22f8ef257cb1ee871
arm64mac=cd6f8691cfd7aa634f21115dd24fa35a41fc02509e714dc3d7c13212bbfadfdb
arm64win=a4a479ae5a21fce937f7a421c36b17c23b0281a6112ebc9dbaceec093839712d
arm64linux=a33b5a4db95eaac5b0d81a93b0d114ab34852feb836dc22e96a4f99f9b33fcf0
arm64musl=a33b5a4db95eaac5b0d81a93b0d114ab34852feb836dc22e96a4f99f9b33fcf0
arm64glibc=a33b5a4db95eaac5b0d81a93b0d114ab34852feb836dc22e96a4f99f9b33fcf0
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
