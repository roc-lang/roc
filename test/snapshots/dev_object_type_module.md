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
x64mac=710e6ab289680e8d9f6ed6b31688053ae72564d4db340fd893a203066dd0756e
x64win=b2a3c9dc1f1351c98b6b032e2ce39dbb9d60b973a765ce8fe3c9f03bb8ee6134
x64freebsd=1c7530988a28a8e5cb3a8b222cf52103477e52d470c8d9e3e2771f78dd5c5578
x64openbsd=1c7530988a28a8e5cb3a8b222cf52103477e52d470c8d9e3e2771f78dd5c5578
x64netbsd=1c7530988a28a8e5cb3a8b222cf52103477e52d470c8d9e3e2771f78dd5c5578
x64musl=1c7530988a28a8e5cb3a8b222cf52103477e52d470c8d9e3e2771f78dd5c5578
x64glibc=1c7530988a28a8e5cb3a8b222cf52103477e52d470c8d9e3e2771f78dd5c5578
x64linux=1c7530988a28a8e5cb3a8b222cf52103477e52d470c8d9e3e2771f78dd5c5578
x64elf=1c7530988a28a8e5cb3a8b222cf52103477e52d470c8d9e3e2771f78dd5c5578
arm64mac=5cb88fa4da271f4fe94dc7dbdd7d2b52eaca7146ff66cc070f9c29f8fc603b5d
arm64win=ce42eac3a1c359f0655d7674cd5d0ff4767365b03a4e01e04372a9658f9699d6
arm64linux=3080d5bd3ff31a1f9cc4928df192a2152b414f51665d1f643de055407e37a799
arm64musl=3080d5bd3ff31a1f9cc4928df192a2152b414f51665d1f643de055407e37a799
arm64glibc=3080d5bd3ff31a1f9cc4928df192a2152b414f51665d1f643de055407e37a799
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
