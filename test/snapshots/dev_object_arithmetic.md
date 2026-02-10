# META
~~~ini
description=Integer arithmetic with I64 return type
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

main : I64
main = add(3, 4) * 2

add : I64, I64 -> I64
add = |a, b| a + b
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : I64 }
    exposes []
    packages {}
    provides { main_for_host: "main" }
    targets: {
        files: "targets/",
        exe: {
            x64glibc: [app],
        }
    }

main_for_host : I64
main_for_host = main
~~~
# MONO
~~~roc
# app
main = 14
add = |a, b| a + b

# platform
main_for_host = <required>

~~~
# DEV OUTPUT
~~~ini
x64mac=a0bbe05fc38302e927cfe755cac1abc66461f1eccb7c283e827f1d35c79c04fe
x64win=568ce81af8f678d1b8e9c051a9f93902bdd52646b1583764e89005754b6f1001
x64freebsd=67a75e4f6cde9382f530fd65f3bc7878f8ca2cb298a41962b9f87204c27d0b9e
x64openbsd=67a75e4f6cde9382f530fd65f3bc7878f8ca2cb298a41962b9f87204c27d0b9e
x64netbsd=67a75e4f6cde9382f530fd65f3bc7878f8ca2cb298a41962b9f87204c27d0b9e
x64musl=67a75e4f6cde9382f530fd65f3bc7878f8ca2cb298a41962b9f87204c27d0b9e
x64glibc=67a75e4f6cde9382f530fd65f3bc7878f8ca2cb298a41962b9f87204c27d0b9e
x64linux=67a75e4f6cde9382f530fd65f3bc7878f8ca2cb298a41962b9f87204c27d0b9e
x64elf=67a75e4f6cde9382f530fd65f3bc7878f8ca2cb298a41962b9f87204c27d0b9e
arm64mac=f519b54ad21444af630daf412809d1379d1cb38cb7d1e836959c21192829311e
arm64win=4b3282684c590dd56e610a2b0af84a79559d9fb8242b71703f28acb60c60b232
arm64linux=810e8a7e070efd1b7fc21313e6f2e404865d76d6c006138c69e17421dc8966a3
arm64musl=810e8a7e070efd1b7fc21313e6f2e404865d76d6c006138c69e17421dc8966a3
arm64glibc=810e8a7e070efd1b7fc21313e6f2e404865d76d6c006138c69e17421dc8966a3
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
