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
# platform
main_for_host = <required>

# app
greeting = "Hello"
name = "World"
main = ""greeting", "name"!"

~~~
# DEV OUTPUT
~~~ini
x64mac=13173e0d8c483ada7122b00bbf9b8274d0f0fd9b948da876d75e2ce10f658cd6
x64win=861e5e120cba53a51ef2a167badccb4ae6f20b3128971cf920d155d9f1412a06
x64freebsd=81389c533bb35b9c5696bcce7332da099e78ee5fbfa4034f0f9e3259ba72cee1
x64openbsd=81389c533bb35b9c5696bcce7332da099e78ee5fbfa4034f0f9e3259ba72cee1
x64netbsd=81389c533bb35b9c5696bcce7332da099e78ee5fbfa4034f0f9e3259ba72cee1
x64musl=81389c533bb35b9c5696bcce7332da099e78ee5fbfa4034f0f9e3259ba72cee1
x64glibc=81389c533bb35b9c5696bcce7332da099e78ee5fbfa4034f0f9e3259ba72cee1
x64linux=81389c533bb35b9c5696bcce7332da099e78ee5fbfa4034f0f9e3259ba72cee1
x64elf=81389c533bb35b9c5696bcce7332da099e78ee5fbfa4034f0f9e3259ba72cee1
arm64mac=e4969ceecd4b5474cccf35605b4fbfe0daebefb746bc1382401bbb56aceccf44
arm64win=4c94adde109663c1c2fab939d52464280a6d015db7ac0fcf2679f415873a3c17
arm64linux=a7132594689d7ba575858034d7179e4cbb70d303fc2e011ccb1f9c97b7b6d011
arm64musl=a7132594689d7ba575858034d7179e4cbb70d303fc2e011ccb1f9c97b7b6d011
arm64glibc=a7132594689d7ba575858034d7179e4cbb70d303fc2e011ccb1f9c97b7b6d011
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
