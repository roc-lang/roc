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
main = "Hello, World!"

# platform
main_for_host = <required>

~~~
# DEV OUTPUT
~~~ini
x64mac=6c42be6e0f89980a34b7eca34b499c3d6c297c63f911dc2cd98389eca7b2f353
x64win=3b0ef1aa5f4afb04f339e7f29c1f4792447a35c607d92ea9b7dd99f7f18b3480
x64freebsd=e07e9d233e4db2ac229d5dc93fb966738edaa24ade09ccdb1f8785cd72ac893f
x64openbsd=e07e9d233e4db2ac229d5dc93fb966738edaa24ade09ccdb1f8785cd72ac893f
x64netbsd=e07e9d233e4db2ac229d5dc93fb966738edaa24ade09ccdb1f8785cd72ac893f
x64musl=e07e9d233e4db2ac229d5dc93fb966738edaa24ade09ccdb1f8785cd72ac893f
x64glibc=e07e9d233e4db2ac229d5dc93fb966738edaa24ade09ccdb1f8785cd72ac893f
x64linux=e07e9d233e4db2ac229d5dc93fb966738edaa24ade09ccdb1f8785cd72ac893f
x64elf=e07e9d233e4db2ac229d5dc93fb966738edaa24ade09ccdb1f8785cd72ac893f
arm64mac=f397d4c7ec6f04bad732176cb2b4ba898a7a416d52259762f7d4cc66d9d21183
arm64win=1f87e97e728bec9de1e70044d172c9590165843e207a4598f95a9f9a729fe491
arm64linux=8d7f159d99e4a4daefcc37464be667ae4708622fc59e3959f52edf5477e4de9f
arm64musl=8d7f159d99e4a4daefcc37464be667ae4708622fc59e3959f52edf5477e4de9f
arm64glibc=8d7f159d99e4a4daefcc37464be667ae4708622fc59e3959f52edf5477e4de9f
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
