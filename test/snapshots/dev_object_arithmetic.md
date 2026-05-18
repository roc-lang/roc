# META
~~~ini
description=Integer arithmetic with I64 return type
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

main : I64
main = add(3, 4) * 2

add : I64, I64 -> I64
add = |a, b| a + b
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : I64 }
    exposes []
    packages {}
    provides { main_for_host: "main" }
    targets: {
        files: "targets/",
        exe: {
            x64glibc: [app],
        }
    }

main_for_host : I64
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# app
main = add(3, 4) * 2
add = |a, b| a + b

~~~
# DEV OUTPUT
~~~ini
x64mac=4953a102309efe7f78bb94052ad469dbafb8e3fe5fbb536174456a6f630e4f89
x64win=72862574fdacefbcd10ee31b471295b24c7f46242ec9e44cab4375cc5172b18b
x64freebsd=4c994a43d2836fd46eeb3d626b12762f4d5da2dd4ed9f0447c56c0dd4e6c278b
x64openbsd=4c994a43d2836fd46eeb3d626b12762f4d5da2dd4ed9f0447c56c0dd4e6c278b
x64netbsd=4c994a43d2836fd46eeb3d626b12762f4d5da2dd4ed9f0447c56c0dd4e6c278b
x64musl=4c994a43d2836fd46eeb3d626b12762f4d5da2dd4ed9f0447c56c0dd4e6c278b
x64glibc=4c994a43d2836fd46eeb3d626b12762f4d5da2dd4ed9f0447c56c0dd4e6c278b
x64linux=4c994a43d2836fd46eeb3d626b12762f4d5da2dd4ed9f0447c56c0dd4e6c278b
x64elf=4c994a43d2836fd46eeb3d626b12762f4d5da2dd4ed9f0447c56c0dd4e6c278b
arm64mac=ab547599402823729c6c67be258cec19c6dbcfe48b583cc93e160a8de93de0ab
arm64win=ac7c5a8bcfcc587fa17a37085c2b3388b9b034c670a12de1b81f3a40a6bc3f79
arm64linux=05d10012b5dbec37f09ffd2661775b6870d286b90a9ece6302a0eb151904f47b
arm64musl=05d10012b5dbec37f09ffd2661775b6870d286b90a9ece6302a0eb151904f47b
arm64glibc=05d10012b5dbec37f09ffd2661775b6870d286b90a9ece6302a0eb151904f47b
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
