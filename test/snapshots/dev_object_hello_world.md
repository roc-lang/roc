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
x64mac=913b359b97ecf9dbe43652fb9bb8eaf1282e8c1274a643ba9b97c9b16c5ddb2a
x64win=8cd2f433861602747b7fc4ff795b46aaf7b606b600c373bcc28b92bebdadf9b1
x64freebsd=5842fc13c99454cea65756d676014152d7f2c3f4fec41c7711a2333833d3acef
x64openbsd=5842fc13c99454cea65756d676014152d7f2c3f4fec41c7711a2333833d3acef
x64netbsd=5842fc13c99454cea65756d676014152d7f2c3f4fec41c7711a2333833d3acef
x64musl=5842fc13c99454cea65756d676014152d7f2c3f4fec41c7711a2333833d3acef
x64glibc=5842fc13c99454cea65756d676014152d7f2c3f4fec41c7711a2333833d3acef
x64linux=5842fc13c99454cea65756d676014152d7f2c3f4fec41c7711a2333833d3acef
x64elf=5842fc13c99454cea65756d676014152d7f2c3f4fec41c7711a2333833d3acef
arm64mac=f397d4c7ec6f04bad732176cb2b4ba898a7a416d52259762f7d4cc66d9d21183
arm64win=1f87e97e728bec9de1e70044d172c9590165843e207a4598f95a9f9a729fe491
arm64linux=8d7f159d99e4a4daefcc37464be667ae4708622fc59e3959f52edf5477e4de9f
arm64musl=8d7f159d99e4a4daefcc37464be667ae4708622fc59e3959f52edf5477e4de9f
arm64glibc=8d7f159d99e4a4daefcc37464be667ae4708622fc59e3959f52edf5477e4de9f
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
