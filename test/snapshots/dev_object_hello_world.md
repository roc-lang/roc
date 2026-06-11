# META
~~~ini
description=Hello world dev object compilation
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

main = "Hello, World!"
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : Str }
    exposes []
    packages {}
    provides { main_for_host: "main" }
    targets: {
        inputs: "targets/",
        x64glibc: { inputs: [app] },
    }

main_for_host : Str
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# app
main = "Hello, World!"

~~~
# DEV OUTPUT
~~~ini
x64mac=dba6fac8e68bf8c577938cd29e6597e3170cb29b546776af6d24e7af87069a76
x64win=861e5e120cba53a51ef2a167badccb4ae6f20b3128971cf920d155d9f1412a06
x64freebsd=527a1225a950027d01d542683d2bce576d6938826a11e900a51a29ad8d1c3c06
x64openbsd=527a1225a950027d01d542683d2bce576d6938826a11e900a51a29ad8d1c3c06
x64netbsd=527a1225a950027d01d542683d2bce576d6938826a11e900a51a29ad8d1c3c06
x64musl=527a1225a950027d01d542683d2bce576d6938826a11e900a51a29ad8d1c3c06
x64glibc=527a1225a950027d01d542683d2bce576d6938826a11e900a51a29ad8d1c3c06
x64linux=527a1225a950027d01d542683d2bce576d6938826a11e900a51a29ad8d1c3c06
x64elf=527a1225a950027d01d542683d2bce576d6938826a11e900a51a29ad8d1c3c06
arm64mac=1df7d59f49761902ad5f1975bb03237e5306454f95d129d2e934892b0e9d4a0b
arm64win=4c94adde109663c1c2fab939d52464280a6d015db7ac0fcf2679f415873a3c17
arm64linux=282ff89940976b3ff3e6dde2e92709acc7b4787e083ce57ebf799a12818d478b
arm64musl=282ff89940976b3ff3e6dde2e92709acc7b4787e083ce57ebf799a12818d478b
arm64glibc=282ff89940976b3ff3e6dde2e92709acc7b4787e083ce57ebf799a12818d478b
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
