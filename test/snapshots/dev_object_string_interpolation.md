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
x64mac=6618ef2591d8892d05cd275bde306bbe10814d2678b72b873c17981193c8867c
x64win=f69824a3d3b9682964a78d5db1ed632ac9bec55560c55b67476d747bbce9b9b8
x64freebsd=989da99358584efcc759c297acf147946e4421fdd326b159c0bf3b8808758fcc
x64openbsd=989da99358584efcc759c297acf147946e4421fdd326b159c0bf3b8808758fcc
x64netbsd=989da99358584efcc759c297acf147946e4421fdd326b159c0bf3b8808758fcc
x64musl=989da99358584efcc759c297acf147946e4421fdd326b159c0bf3b8808758fcc
x64glibc=989da99358584efcc759c297acf147946e4421fdd326b159c0bf3b8808758fcc
x64linux=989da99358584efcc759c297acf147946e4421fdd326b159c0bf3b8808758fcc
x64elf=989da99358584efcc759c297acf147946e4421fdd326b159c0bf3b8808758fcc
arm64mac=ab86db92a1f3273b8637083fe830af504faa4471ead2b02d76da48451b19df89
arm64win=beca763ab7efe33cc1b5a12fd5e8cf900dabdff7a6638787b894f811b1588a28
arm64linux=540233323d8c63467695cd98f6e0ecf4bce1a46c3d2e0aea95c5f54d12e7c2a6
arm64musl=540233323d8c63467695cd98f6e0ecf4bce1a46c3d2e0aea95c5f54d12e7c2a6
arm64glibc=540233323d8c63467695cd98f6e0ecf4bce1a46c3d2e0aea95c5f54d12e7c2a6
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
