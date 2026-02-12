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
x64mac=ac63764f4dd71ae4262a7d77907d7467301e9d957db1749e3e481ab8be6f2cf4
x64win=73ff39ee43a6b191b2b1099806796e4b786c90bab29f16a696694a02129e3f4a
x64freebsd=27164151221f0081726d3ac98d505b95001cc6ea591ee26bb99be61b833baabd
x64openbsd=27164151221f0081726d3ac98d505b95001cc6ea591ee26bb99be61b833baabd
x64netbsd=27164151221f0081726d3ac98d505b95001cc6ea591ee26bb99be61b833baabd
x64musl=27164151221f0081726d3ac98d505b95001cc6ea591ee26bb99be61b833baabd
x64glibc=27164151221f0081726d3ac98d505b95001cc6ea591ee26bb99be61b833baabd
x64linux=27164151221f0081726d3ac98d505b95001cc6ea591ee26bb99be61b833baabd
x64elf=27164151221f0081726d3ac98d505b95001cc6ea591ee26bb99be61b833baabd
arm64mac=53c72b54063cc45b1a8876eaa980e8f74cb06774ad4e4f53dbe844133cb6629e
arm64win=9f160a143e28ead27ca95f8a8ef6f701d73e071278ee21ead5313b6924f0d456
arm64linux=5bb11ab958180420f9fada534d473fcdba86d6eef3bb84b2ca3ab8b84e5bee75
arm64musl=5bb11ab958180420f9fada534d473fcdba86d6eef3bb84b2ca3ab8b84e5bee75
arm64glibc=5bb11ab958180420f9fada534d473fcdba86d6eef3bb84b2ca3ab8b84e5bee75
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
