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
x64mac=280b821005f127bbfb50e792a4f71155454a05f6e837e03c57ffb803264a15c9
x64win=f67acc767a12923636ee1526236011d2d13c9c4471b7af152a6862271790f6c3
x64freebsd=9d37d8271b8fd16ff84e1a06ec0533a9a9a989ec299f3fefb1cfb7cdc078eb01
x64openbsd=9d37d8271b8fd16ff84e1a06ec0533a9a9a989ec299f3fefb1cfb7cdc078eb01
x64netbsd=9d37d8271b8fd16ff84e1a06ec0533a9a9a989ec299f3fefb1cfb7cdc078eb01
x64musl=9d37d8271b8fd16ff84e1a06ec0533a9a9a989ec299f3fefb1cfb7cdc078eb01
x64glibc=9d37d8271b8fd16ff84e1a06ec0533a9a9a989ec299f3fefb1cfb7cdc078eb01
x64linux=9d37d8271b8fd16ff84e1a06ec0533a9a9a989ec299f3fefb1cfb7cdc078eb01
x64elf=9d37d8271b8fd16ff84e1a06ec0533a9a9a989ec299f3fefb1cfb7cdc078eb01
arm64mac=8533fe5e6b7313ad996c15ae480ff4c2fb675f7465f256ecadf21dd46874a6dc
arm64win=a639e700098577549934b7748067bf954ae4d4168d82e4c7e62d58f2eeb04e5e
arm64linux=67eed12e093f47ec3b759119586b45d5ee432b97b6d9e01210d05fe60afd5527
arm64musl=67eed12e093f47ec3b759119586b45d5ee432b97b6d9e01210d05fe60afd5527
arm64glibc=67eed12e093f47ec3b759119586b45d5ee432b97b6d9e01210d05fe60afd5527
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
