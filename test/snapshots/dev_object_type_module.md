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
x64mac=a44f22e9d7f5cc87d278902ba65b8e27fcd01a6e0e55ae752bf7b7944f6efdcf
x64win=a2691c2bde3762299833bc063e8864ca0c18c92ee1123314a5b165f72c135ec9
x64freebsd=4640f6b9953066c8590395cb2f9de26a1bcd6fc1572501d18dbcafa19465f4bd
x64openbsd=4640f6b9953066c8590395cb2f9de26a1bcd6fc1572501d18dbcafa19465f4bd
x64netbsd=4640f6b9953066c8590395cb2f9de26a1bcd6fc1572501d18dbcafa19465f4bd
x64musl=4640f6b9953066c8590395cb2f9de26a1bcd6fc1572501d18dbcafa19465f4bd
x64glibc=4640f6b9953066c8590395cb2f9de26a1bcd6fc1572501d18dbcafa19465f4bd
x64linux=4640f6b9953066c8590395cb2f9de26a1bcd6fc1572501d18dbcafa19465f4bd
x64elf=4640f6b9953066c8590395cb2f9de26a1bcd6fc1572501d18dbcafa19465f4bd
arm64mac=d41a901dfab2efbd8296214188e49eadca0f9e507d7e75ad2df749e3b99f8e70
arm64win=76ae0fd4c8d8611680978619613a2f9c1a23e759651800e0d0f8ce6ed17169d0
arm64linux=2cdebf0409373db4dc93de7f787385a01cb3b22c3be95ef3f903aabc9bb85ab0
arm64musl=2cdebf0409373db4dc93de7f787385a01cb3b22c3be95ef3f903aabc9bb85ab0
arm64glibc=2cdebf0409373db4dc93de7f787385a01cb3b22c3be95ef3f903aabc9bb85ab0
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
