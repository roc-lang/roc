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
x64mac=61c5dc41e065f6542d203d6d66a5a92464f72983ca79907741459a4159bf31de
x64win=2a214110e9bc6cf6a36c76cd066cf14e309cca4d975ced4da9da8fccd1d6e7c7
x64freebsd=46ad0a875bf3f89131d5f03daa39bd3d7d439549574a29a247ac2161f1fbc6ca
x64openbsd=46ad0a875bf3f89131d5f03daa39bd3d7d439549574a29a247ac2161f1fbc6ca
x64netbsd=46ad0a875bf3f89131d5f03daa39bd3d7d439549574a29a247ac2161f1fbc6ca
x64musl=46ad0a875bf3f89131d5f03daa39bd3d7d439549574a29a247ac2161f1fbc6ca
x64glibc=46ad0a875bf3f89131d5f03daa39bd3d7d439549574a29a247ac2161f1fbc6ca
x64linux=46ad0a875bf3f89131d5f03daa39bd3d7d439549574a29a247ac2161f1fbc6ca
x64elf=46ad0a875bf3f89131d5f03daa39bd3d7d439549574a29a247ac2161f1fbc6ca
arm64mac=a0c538f9f4d4f98fe222d4e5500840fed0f3bb5e14dc7b40775523e5f188ceb5
arm64win=c11bbc92fb124548dd9dbcf18195c9d68b8bd3979230467274754d706ea118ef
arm64linux=f2596ac74782dfd71ad12cfe5316ad0fe69566717848b4bb0a8fad8d99e29efe
arm64musl=f2596ac74782dfd71ad12cfe5316ad0fe69566717848b4bb0a8fad8d99e29efe
arm64glibc=f2596ac74782dfd71ad12cfe5316ad0fe69566717848b4bb0a8fad8d99e29efe
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
