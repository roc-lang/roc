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
arm64mac=355302e7f40504de3cdab36b5c92f370958ca6629f5655cc4642e6594fda6e9a
arm64win=adc6f0883f396b0f59bb4b4c3c4666cd319df94f6a6f0641b18cbc732eb5cc27
arm64linux=958db6aad587af20a1acd916280b3d353df8d8b30d1e95f746b255b3cf7a3ac3
arm64musl=958db6aad587af20a1acd916280b3d353df8d8b30d1e95f746b255b3cf7a3ac3
arm64glibc=958db6aad587af20a1acd916280b3d353df8d8b30d1e95f746b255b3cf7a3ac3
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
