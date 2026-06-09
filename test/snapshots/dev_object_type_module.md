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
            x64glibc: { files: [app] },
        }
    }

main_for_host : Str
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# Color
red = Red
green = Green
blue = Blue
to_str = |color| match color {
	Red => "red"
	Green => "green"
	Blue => "blue"
}

# app
main = to_str(red)

~~~
# DEV OUTPUT
~~~ini
x64mac=33ae7d7a5614e165e67d84785f61f0a1b4479a983396efd22f612db8410424ce
x64win=1daef17641e043f3ad6337cdf62da40514a74d4dbd0790db66fb5b2a8d322c32
x64freebsd=fa641cac78517ba38813bc2e114656cb3a596bd6b9ddcfd2f67492ebd4f98c1c
x64openbsd=fa641cac78517ba38813bc2e114656cb3a596bd6b9ddcfd2f67492ebd4f98c1c
x64netbsd=fa641cac78517ba38813bc2e114656cb3a596bd6b9ddcfd2f67492ebd4f98c1c
x64musl=fa641cac78517ba38813bc2e114656cb3a596bd6b9ddcfd2f67492ebd4f98c1c
x64glibc=fa641cac78517ba38813bc2e114656cb3a596bd6b9ddcfd2f67492ebd4f98c1c
x64linux=fa641cac78517ba38813bc2e114656cb3a596bd6b9ddcfd2f67492ebd4f98c1c
x64elf=fa641cac78517ba38813bc2e114656cb3a596bd6b9ddcfd2f67492ebd4f98c1c
arm64mac=9bc95738c715b239e4c4cb34a91276a0115c8ca5eb5d5e752e6e225d24fc71e3
arm64win=c25f1af5449d9203ccb6047a467adccde3caee7f094cfa7ddb0a7f7d201ba658
arm64linux=f9fb7662f677335bcf184db521402e885758f488c976235f2da3c0eb81b1d622
arm64musl=f9fb7662f677335bcf184db521402e885758f488c976235f2da3c0eb81b1d622
arm64glibc=f9fb7662f677335bcf184db521402e885758f488c976235f2da3c0eb81b1d622
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
