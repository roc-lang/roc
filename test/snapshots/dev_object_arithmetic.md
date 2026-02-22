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
arm64mac=48603576ddb474e07b0b0ca13ef5323595fc020c7f2a489bcccbf8de384ef0d4
arm64win=77842ad341c3b7ca15c254ccbba9fa7e02a9cca070b85b33753b2c1134be0e39
arm64linux=0e1ccab472f418863c2c4ce7f556ee25b07ca8c45e36b69726f9809087796d28
arm64musl=0e1ccab472f418863c2c4ce7f556ee25b07ca8c45e36b69726f9809087796d28
arm64glibc=0e1ccab472f418863c2c4ce7f556ee25b07ca8c45e36b69726f9809087796d28
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
