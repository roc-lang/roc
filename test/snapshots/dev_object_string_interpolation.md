# META
~~~ini
description=String interpolation and concatenation
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

greeting = "Hello"
name = "World"
main = "${greeting}, ${name}!"
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
greeting = "Hello"
name = "World"
main = ""greeting", "name"!"

# platform
main_for_host = <required>

~~~
# DEV OUTPUT
~~~ini
x64mac=5b70b1056fc5ea3aee415609bb021dc298998dd87dbb931b1033b91f8ed6c004
x64win=7b6fa2e633c120f2e87c1a80da93959b88e310cda7fb03958c5d34ac308bbe65
x64freebsd=3eef697471acd437ae885fead637328e8ff68ff731f3b171247d4c8801b99fe4
x64openbsd=3eef697471acd437ae885fead637328e8ff68ff731f3b171247d4c8801b99fe4
x64netbsd=3eef697471acd437ae885fead637328e8ff68ff731f3b171247d4c8801b99fe4
x64musl=3eef697471acd437ae885fead637328e8ff68ff731f3b171247d4c8801b99fe4
x64glibc=3eef697471acd437ae885fead637328e8ff68ff731f3b171247d4c8801b99fe4
x64linux=3eef697471acd437ae885fead637328e8ff68ff731f3b171247d4c8801b99fe4
x64elf=3eef697471acd437ae885fead637328e8ff68ff731f3b171247d4c8801b99fe4
arm64mac=7e71701a8b4d1f6ef306058f45b7fb855db6d8e01523c3f413f4457dd3a888fb
arm64win=d524cf534647200f7cd7278424baa7e80fabd05dbc6fa2ec9784dfa433e39f52
arm64linux=a1cd84536fc08aad5139a27240b814b91c30f259d68a559d61907d804d1a0835
arm64musl=a1cd84536fc08aad5139a27240b814b91c30f259d68a559d61907d804d1a0835
arm64glibc=a1cd84536fc08aad5139a27240b814b91c30f259d68a559d61907d804d1a0835
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
