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
x64mac=c9bcbeb13296f7daadd6d64096bb363c8a38aa49bdd52160b3b6f509c03c568f
x64win=9589a3da344fbea92b7010f4fb6bb9f13ff6545e8e51dbde4e4d6f005c549608
x64freebsd=ee27b23571794335af1e2c20b5f8d2881803ce09541898346c320bd13115a175
x64openbsd=ee27b23571794335af1e2c20b5f8d2881803ce09541898346c320bd13115a175
x64netbsd=ee27b23571794335af1e2c20b5f8d2881803ce09541898346c320bd13115a175
x64musl=ee27b23571794335af1e2c20b5f8d2881803ce09541898346c320bd13115a175
x64glibc=ee27b23571794335af1e2c20b5f8d2881803ce09541898346c320bd13115a175
x64linux=ee27b23571794335af1e2c20b5f8d2881803ce09541898346c320bd13115a175
x64elf=ee27b23571794335af1e2c20b5f8d2881803ce09541898346c320bd13115a175
arm64mac=f61334a16fc0ba725c065f09fdcaf7460f0dd5b3be12e7c5c6ca1b531f808c11
arm64win=2e74f731859e7c3ac7590866f50762e14271109f84a37ee5cc2d5426fdd7637f
arm64linux=66b22f61f09f04cd3ac5e858d64668af240fc426b710d5109f679fbc72cd2264
arm64musl=66b22f61f09f04cd3ac5e858d64668af240fc426b710d5109f679fbc72cd2264
arm64glibc=66b22f61f09f04cd3ac5e858d64668af240fc426b710d5109f679fbc72cd2264
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
