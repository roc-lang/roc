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
x64mac=81e1ff628868460f55fa72feb48113284c2028604e6336334ba9a661d5e5a74f
x64win=861e5e120cba53a51ef2a167badccb4ae6f20b3128971cf920d155d9f1412a06
x64freebsd=bcc68ec897b0591814c2e31919a472fe88689803bb00147a4a64dcde22ff0019
x64openbsd=bcc68ec897b0591814c2e31919a472fe88689803bb00147a4a64dcde22ff0019
x64netbsd=bcc68ec897b0591814c2e31919a472fe88689803bb00147a4a64dcde22ff0019
x64musl=bcc68ec897b0591814c2e31919a472fe88689803bb00147a4a64dcde22ff0019
x64glibc=bcc68ec897b0591814c2e31919a472fe88689803bb00147a4a64dcde22ff0019
x64linux=bcc68ec897b0591814c2e31919a472fe88689803bb00147a4a64dcde22ff0019
x64elf=bcc68ec897b0591814c2e31919a472fe88689803bb00147a4a64dcde22ff0019
arm64mac=9f8f806b4fead6e4c37eea60d85f7cee82fdba06b766b67a8cdaea2f0605a71f
arm64win=4c94adde109663c1c2fab939d52464280a6d015db7ac0fcf2679f415873a3c17
arm64linux=b39f4620f9114a258d1ae604d4d39af0c457ce2787896f3cbf6d949cb53e0e4a
arm64musl=b39f4620f9114a258d1ae604d4d39af0c457ce2787896f3cbf6d949cb53e0e4a
arm64glibc=b39f4620f9114a258d1ae604d4d39af0c457ce2787896f3cbf6d949cb53e0e4a
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
