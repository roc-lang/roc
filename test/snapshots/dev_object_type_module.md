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
x64mac=07845c3c945c2ccc9579c8fa75af1be878104af08ae26f3ac27baa2be8447aed
x64win=e4c4986063fed4502fa475004642e8d284ff46fb6704b43810796dca9093b63f
x64freebsd=3fd8187697bf50b12f9e37825fd843950d6b29f8f8837d6bff053b14db506285
x64openbsd=3fd8187697bf50b12f9e37825fd843950d6b29f8f8837d6bff053b14db506285
x64netbsd=3fd8187697bf50b12f9e37825fd843950d6b29f8f8837d6bff053b14db506285
x64musl=3fd8187697bf50b12f9e37825fd843950d6b29f8f8837d6bff053b14db506285
x64glibc=3fd8187697bf50b12f9e37825fd843950d6b29f8f8837d6bff053b14db506285
x64linux=3fd8187697bf50b12f9e37825fd843950d6b29f8f8837d6bff053b14db506285
x64elf=3fd8187697bf50b12f9e37825fd843950d6b29f8f8837d6bff053b14db506285
arm64mac=cd6f8691cfd7aa634f21115dd24fa35a41fc02509e714dc3d7c13212bbfadfdb
arm64win=a4a479ae5a21fce937f7a421c36b17c23b0281a6112ebc9dbaceec093839712d
arm64linux=a33b5a4db95eaac5b0d81a93b0d114ab34852feb836dc22e96a4f99f9b33fcf0
arm64musl=a33b5a4db95eaac5b0d81a93b0d114ab34852feb836dc22e96a4f99f9b33fcf0
arm64glibc=a33b5a4db95eaac5b0d81a93b0d114ab34852feb836dc22e96a4f99f9b33fcf0
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
