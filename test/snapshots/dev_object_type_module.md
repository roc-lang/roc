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
x64mac=7fed815b935ab6aa82a857f5e72c49e56ffdc5a481ba5c32b09200fb9590743f
x64win=e0817973765a254cad62aebe37247d7e79f0ab422a53d36d8a8002db23e70e50
x64freebsd=2484d314aa918f27d69517128dacc0ba9138e64e87198690d74991cd10fb17ff
x64openbsd=2484d314aa918f27d69517128dacc0ba9138e64e87198690d74991cd10fb17ff
x64netbsd=2484d314aa918f27d69517128dacc0ba9138e64e87198690d74991cd10fb17ff
x64musl=2484d314aa918f27d69517128dacc0ba9138e64e87198690d74991cd10fb17ff
x64glibc=2484d314aa918f27d69517128dacc0ba9138e64e87198690d74991cd10fb17ff
x64linux=2484d314aa918f27d69517128dacc0ba9138e64e87198690d74991cd10fb17ff
x64elf=2484d314aa918f27d69517128dacc0ba9138e64e87198690d74991cd10fb17ff
arm64mac=9fb9f4c5d7628ac64604833e400868db735e5602a125153fa4afc7ae19e29e25
arm64win=dc57fe53e93fc9febf564f59069508acb0b4f741c51c4878a1d4ea57724d752a
arm64linux=5a1cbe09f6a7370dcf71af159ec2e60be006696e4a44898501cf2dc4421fe262
arm64musl=5a1cbe09f6a7370dcf71af159ec2e60be006696e4a44898501cf2dc4421fe262
arm64glibc=5a1cbe09f6a7370dcf71af159ec2e60be006696e4a44898501cf2dc4421fe262
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
