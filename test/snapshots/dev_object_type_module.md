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
red = Red
green = Green
blue = Blue
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
x64mac=18b23a5eab0308b56894fe5134d5277b32b87df939a1ba0dae49a61dfcbb5412
x64win=4fb04fb68662197d0de5e2a84d8585bf715459a5dfaf522fface0ac5b42a3a43
x64freebsd=c91bdfc427807e1df3e81d7216642ee2f42066ac1a79707a066fb25d8017cdc2
x64openbsd=c91bdfc427807e1df3e81d7216642ee2f42066ac1a79707a066fb25d8017cdc2
x64netbsd=c91bdfc427807e1df3e81d7216642ee2f42066ac1a79707a066fb25d8017cdc2
x64musl=c91bdfc427807e1df3e81d7216642ee2f42066ac1a79707a066fb25d8017cdc2
x64glibc=c91bdfc427807e1df3e81d7216642ee2f42066ac1a79707a066fb25d8017cdc2
x64linux=c91bdfc427807e1df3e81d7216642ee2f42066ac1a79707a066fb25d8017cdc2
x64elf=c91bdfc427807e1df3e81d7216642ee2f42066ac1a79707a066fb25d8017cdc2
arm64mac=e58379bf4437258ee1447b42709c2a10e9fb0ad476449e672060b05626dedf31
arm64win=8010fafe89fe30e6f4ab7500a72849a09e413cdc53c633f0e71d0abd69433aad
arm64linux=c051ca1d15c16c9cd6d247682cd281dbc300eb5e3dc742ba1feb64bac15115a1
arm64musl=c051ca1d15c16c9cd6d247682cd281dbc300eb5e3dc742ba1feb64bac15115a1
arm64glibc=c051ca1d15c16c9cd6d247682cd281dbc300eb5e3dc742ba1feb64bac15115a1
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
