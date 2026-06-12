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
    provides { "roc_main": main_for_host }
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
x64mac=1d3cffb8b7df73d01d967ad865edd7f906c1ec6c24cdc1b6837af29b61acce90
x64win=8d4a4dc0ce2a4a812bfea9b94eb605c8731f7ee23786884214c86859b5ea4152
x64freebsd=59a6af861290aca00ba9b526e243bb62774cfc840b14eee89573e857cbe07460
x64openbsd=59a6af861290aca00ba9b526e243bb62774cfc840b14eee89573e857cbe07460
x64netbsd=59a6af861290aca00ba9b526e243bb62774cfc840b14eee89573e857cbe07460
x64musl=59a6af861290aca00ba9b526e243bb62774cfc840b14eee89573e857cbe07460
x64glibc=59a6af861290aca00ba9b526e243bb62774cfc840b14eee89573e857cbe07460
x64linux=59a6af861290aca00ba9b526e243bb62774cfc840b14eee89573e857cbe07460
x64elf=59a6af861290aca00ba9b526e243bb62774cfc840b14eee89573e857cbe07460
arm64mac=7911fd5973536b6d2fb96daebbb32f4865949dad4647115c310eb004e9461c27
arm64win=88d22672812a1a8b92d1f2cde697d98c61c8c8bd04807c28447d01637c4dbace
arm64linux=155b75e78801e084410e6c5a99c6045a96462630266881328b9892d1daa82beb
arm64musl=155b75e78801e084410e6c5a99c6045a96462630266881328b9892d1daa82beb
arm64glibc=155b75e78801e084410e6c5a99c6045a96462630266881328b9892d1daa82beb
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
