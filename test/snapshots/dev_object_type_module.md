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
x64mac=f279b9f7fffdd27566a6e6bbec15610c1e6db64e4eca94aff8acf2a6412e83c4
x64win=8c10852926c3653c40589d2329eebc246414432f92000af15728ff6b43fce72c
x64freebsd=89743914d112793d108931af2c6336851530b540a9d456ef07d9ea1d5ad0f68c
x64openbsd=89743914d112793d108931af2c6336851530b540a9d456ef07d9ea1d5ad0f68c
x64netbsd=89743914d112793d108931af2c6336851530b540a9d456ef07d9ea1d5ad0f68c
x64musl=89743914d112793d108931af2c6336851530b540a9d456ef07d9ea1d5ad0f68c
x64glibc=89743914d112793d108931af2c6336851530b540a9d456ef07d9ea1d5ad0f68c
x64linux=89743914d112793d108931af2c6336851530b540a9d456ef07d9ea1d5ad0f68c
x64elf=89743914d112793d108931af2c6336851530b540a9d456ef07d9ea1d5ad0f68c
arm64mac=9fb9f4c5d7628ac64604833e400868db735e5602a125153fa4afc7ae19e29e25
arm64win=dc57fe53e93fc9febf564f59069508acb0b4f741c51c4878a1d4ea57724d752a
arm64linux=5a1cbe09f6a7370dcf71af159ec2e60be006696e4a44898501cf2dc4421fe262
arm64musl=5a1cbe09f6a7370dcf71af159ec2e60be006696e4a44898501cf2dc4421fe262
arm64glibc=5a1cbe09f6a7370dcf71af159ec2e60be006696e4a44898501cf2dc4421fe262
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
