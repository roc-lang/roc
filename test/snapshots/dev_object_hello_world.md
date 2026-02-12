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
x64mac=921537feb2ec1dcbe3c73e21d751c1e23af9f02e62702cacb075f4e5eb76a6d1
x64win=ef923ca5a4841d7851eb8d65ca8f56eb68cbf49b1dba7c6a032d11029635d93e
x64freebsd=382cc4a651ac5da1c1c1c963502b2e0373dca4a301513a51be0612a7c2334f5f
x64openbsd=382cc4a651ac5da1c1c1c963502b2e0373dca4a301513a51be0612a7c2334f5f
x64netbsd=382cc4a651ac5da1c1c1c963502b2e0373dca4a301513a51be0612a7c2334f5f
x64musl=382cc4a651ac5da1c1c1c963502b2e0373dca4a301513a51be0612a7c2334f5f
x64glibc=382cc4a651ac5da1c1c1c963502b2e0373dca4a301513a51be0612a7c2334f5f
x64linux=382cc4a651ac5da1c1c1c963502b2e0373dca4a301513a51be0612a7c2334f5f
x64elf=382cc4a651ac5da1c1c1c963502b2e0373dca4a301513a51be0612a7c2334f5f
arm64mac=96a46dbea12169e4b0fb50a53de64835d8b51a63ab308b4208c3da52473e12f7
arm64win=fae3d6639927123d17eee094c1a23dca6d7c35255fd808c56b9ca5306b6e81bf
arm64linux=e69183b6bdeda68ed8486fd827322d92937d4f8185c7beb6d93d7689f689b717
arm64musl=e69183b6bdeda68ed8486fd827322d92937d4f8185c7beb6d93d7689f689b717
arm64glibc=e69183b6bdeda68ed8486fd827322d92937d4f8185c7beb6d93d7689f689b717
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
