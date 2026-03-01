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
x64mac=808677b4934edcdff34bd5489829e70b3745fc18b5224c1523436a41d7481e70
x64win=e62e9a4b0c64511a29c19416541633fde84afba6917a0fdab95fc177b5f2efe4
x64freebsd=7688b74b76401d5287cf7f62f806eeb710895a0494df3edc767b86ab0ed452cc
x64openbsd=7688b74b76401d5287cf7f62f806eeb710895a0494df3edc767b86ab0ed452cc
x64netbsd=7688b74b76401d5287cf7f62f806eeb710895a0494df3edc767b86ab0ed452cc
x64musl=7688b74b76401d5287cf7f62f806eeb710895a0494df3edc767b86ab0ed452cc
x64glibc=7688b74b76401d5287cf7f62f806eeb710895a0494df3edc767b86ab0ed452cc
x64linux=7688b74b76401d5287cf7f62f806eeb710895a0494df3edc767b86ab0ed452cc
x64elf=7688b74b76401d5287cf7f62f806eeb710895a0494df3edc767b86ab0ed452cc
arm64mac=04d1644fca081c8bf9476965b92ed49438b025bd4db0d606a24593ca5d1bddcf
arm64win=a2b18b822b525a46656bc99f132cd450c26e43c809c1284b4bfdfd2743b4423c
arm64linux=42395dbb23a244940abb04739cfc9dce8f0ccc9dac18c4ec458217ad88215f12
arm64musl=42395dbb23a244940abb04739cfc9dce8f0ccc9dac18c4ec458217ad88215f12
arm64glibc=42395dbb23a244940abb04739cfc9dce8f0ccc9dac18c4ec458217ad88215f12
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
