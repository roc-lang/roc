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
arm64mac=a01b2cac8b5bec06dc686c72d19ade9c7f237470e18b1bb2494af819c71d2f11
arm64win=648b3c283b07d055ac7a8deac9b7a0e79f09642c1847d726f343200b8ae915e7
arm64linux=f5b331fe7326038957b6f62ae5647702e565f97da721361f1c5a247efa20ae0d
arm64musl=f5b331fe7326038957b6f62ae5647702e565f97da721361f1c5a247efa20ae0d
arm64glibc=f5b331fe7326038957b6f62ae5647702e565f97da721361f1c5a247efa20ae0d
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
