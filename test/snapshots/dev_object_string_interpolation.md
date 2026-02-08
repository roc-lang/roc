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
x64mac=058d3fd800914bb3b6cd681bb768378d1bae4cc6e19694bf9aa0bb1db061dedb
x64win=a38f6bec5f6817a0847ba6119fd98042413d81b8bee17a290989b7739906efdc
x64freebsd=69f569b30c22ab47088cffab49d615439118c7c8414d385b69e6fb74bef769a3
x64openbsd=69f569b30c22ab47088cffab49d615439118c7c8414d385b69e6fb74bef769a3
x64netbsd=69f569b30c22ab47088cffab49d615439118c7c8414d385b69e6fb74bef769a3
x64musl=69f569b30c22ab47088cffab49d615439118c7c8414d385b69e6fb74bef769a3
x64glibc=69f569b30c22ab47088cffab49d615439118c7c8414d385b69e6fb74bef769a3
x64linux=69f569b30c22ab47088cffab49d615439118c7c8414d385b69e6fb74bef769a3
x64elf=69f569b30c22ab47088cffab49d615439118c7c8414d385b69e6fb74bef769a3
arm64mac=7e71701a8b4d1f6ef306058f45b7fb855db6d8e01523c3f413f4457dd3a888fb
arm64win=d524cf534647200f7cd7278424baa7e80fabd05dbc6fa2ec9784dfa433e39f52
arm64linux=a1cd84536fc08aad5139a27240b814b91c30f259d68a559d61907d804d1a0835
arm64musl=a1cd84536fc08aad5139a27240b814b91c30f259d68a559d61907d804d1a0835
arm64glibc=a1cd84536fc08aad5139a27240b814b91c30f259d68a559d61907d804d1a0835
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
