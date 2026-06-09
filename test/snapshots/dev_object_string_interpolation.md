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
            x64glibc: { files: [app] },
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
x64freebsd=bcc68ec897b0591814c2e31919a472fe88689803bb00147a4a64dcde22ff0019
x64openbsd=bcc68ec897b0591814c2e31919a472fe88689803bb00147a4a64dcde22ff0019
x64netbsd=bcc68ec897b0591814c2e31919a472fe88689803bb00147a4a64dcde22ff0019
x64musl=bcc68ec897b0591814c2e31919a472fe88689803bb00147a4a64dcde22ff0019
x64glibc=bcc68ec897b0591814c2e31919a472fe88689803bb00147a4a64dcde22ff0019
x64linux=bcc68ec897b0591814c2e31919a472fe88689803bb00147a4a64dcde22ff0019
x64elf=bcc68ec897b0591814c2e31919a472fe88689803bb00147a4a64dcde22ff0019
arm64mac=e4969ceecd4b5474cccf35605b4fbfe0daebefb746bc1382401bbb56aceccf44
arm64win=4c94adde109663c1c2fab939d52464280a6d015db7ac0fcf2679f415873a3c17
arm64linux=b39f4620f9114a258d1ae604d4d39af0c457ce2787896f3cbf6d949cb53e0e4a
arm64musl=b39f4620f9114a258d1ae604d4d39af0c457ce2787896f3cbf6d949cb53e0e4a
arm64glibc=b39f4620f9114a258d1ae604d4d39af0c457ce2787896f3cbf6d949cb53e0e4a
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
