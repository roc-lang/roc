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
x64mac=c6dac636f4d244911b8d71dbf595f5aa3f9f0734adba126dc27f4afb7bdad184
x64win=ce322a1b311951b2d273d81da90750245460aac3bc647023b80678355fc40cfb
x64freebsd=a159959de56bede4c405a6ddf9b07a910d3fb4a99fa895a086d537752d166a47
x64openbsd=a159959de56bede4c405a6ddf9b07a910d3fb4a99fa895a086d537752d166a47
x64netbsd=a159959de56bede4c405a6ddf9b07a910d3fb4a99fa895a086d537752d166a47
x64musl=a159959de56bede4c405a6ddf9b07a910d3fb4a99fa895a086d537752d166a47
x64glibc=a159959de56bede4c405a6ddf9b07a910d3fb4a99fa895a086d537752d166a47
x64linux=a159959de56bede4c405a6ddf9b07a910d3fb4a99fa895a086d537752d166a47
x64elf=a159959de56bede4c405a6ddf9b07a910d3fb4a99fa895a086d537752d166a47
arm64mac=05192279f481aba67bdd629b72f67e82577b0f42d094d0087da26029a9bc9adc
arm64win=92126531c9c1eb7cc3ff88b3f0d7aea58997402568927ca29fb4868de52e0422
arm64linux=b4ddbe0b7ad3b960959a2a67632d2a371d5616a3e9b967821510f72f56ed2811
arm64musl=b4ddbe0b7ad3b960959a2a67632d2a371d5616a3e9b967821510f72f56ed2811
arm64glibc=b4ddbe0b7ad3b960959a2a67632d2a371d5616a3e9b967821510f72f56ed2811
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
