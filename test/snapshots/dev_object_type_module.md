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
arm64mac=d6a10096e86c8ac84b4999488ef1a20eb45b6173e988d34ea4f075da316c07cc
arm64win=c8ffc5e319888b1fd99a386bbc8703e0b89a64c5e57aac0fb7f0cad826952d60
arm64linux=12aedb8f108213f7f9964fd1f3663e1bd809f615970053d839f4bb316404e1b4
arm64musl=12aedb8f108213f7f9964fd1f3663e1bd809f615970053d839f4bb316404e1b4
arm64glibc=12aedb8f108213f7f9964fd1f3663e1bd809f615970053d839f4bb316404e1b4
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
