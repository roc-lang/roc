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
x64mac=26cf30acce2b5c36248330c25788f67c9f60d58113eae66849ccdb1feed2526f
x64win=220fbeb2765c75e774bd8f540a0cfc4d65f9399411e939f414f0ba2ff33f88c1
x64freebsd=00f759ac38a51259e78ed1dabeaeeaedcc1ad6c0cfb0156fa2c9883bf31527e7
x64openbsd=00f759ac38a51259e78ed1dabeaeeaedcc1ad6c0cfb0156fa2c9883bf31527e7
x64netbsd=00f759ac38a51259e78ed1dabeaeeaedcc1ad6c0cfb0156fa2c9883bf31527e7
x64musl=00f759ac38a51259e78ed1dabeaeeaedcc1ad6c0cfb0156fa2c9883bf31527e7
x64glibc=00f759ac38a51259e78ed1dabeaeeaedcc1ad6c0cfb0156fa2c9883bf31527e7
x64linux=00f759ac38a51259e78ed1dabeaeeaedcc1ad6c0cfb0156fa2c9883bf31527e7
x64elf=00f759ac38a51259e78ed1dabeaeeaedcc1ad6c0cfb0156fa2c9883bf31527e7
arm64mac=7e71701a8b4d1f6ef306058f45b7fb855db6d8e01523c3f413f4457dd3a888fb
arm64win=cbb8e6bdd34b6e0e5fd868cc449bdaec4a7b72bf4b692c380ced85aa88b89d03
arm64linux=a1cd84536fc08aad5139a27240b814b91c30f259d68a559d61907d804d1a0835
arm64musl=a1cd84536fc08aad5139a27240b814b91c30f259d68a559d61907d804d1a0835
arm64glibc=a1cd84536fc08aad5139a27240b814b91c30f259d68a559d61907d804d1a0835
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
