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
    provides { "roc_main": main_for_host }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
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
x64mac=78df2b07056beb3bb8c4d2d04fb7aca411b6767bd886422a6e9c8bf816870d48
x64win=d286ad7c93561a310b64656d24d94ef6947b69f04b0fa9d5d9f9821561861022
x64mingw=d286ad7c93561a310b64656d24d94ef6947b69f04b0fa9d5d9f9821561861022
x64freebsd=268bf27fa5c42bbd9303c6dd4155877503f4102cd2514cb73bbc10c74334e7ad
x64openbsd=a0dcd7fa771891fee2cab6eecbb22ac2b94894d409697a24ac33f44a29c8538e
x64netbsd=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64musl=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64glibc=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64linux=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64elf=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64v1mac=78df2b07056beb3bb8c4d2d04fb7aca411b6767bd886422a6e9c8bf816870d48
x64v1win=d286ad7c93561a310b64656d24d94ef6947b69f04b0fa9d5d9f9821561861022
x64v1mingw=d286ad7c93561a310b64656d24d94ef6947b69f04b0fa9d5d9f9821561861022
x64v1freebsd=268bf27fa5c42bbd9303c6dd4155877503f4102cd2514cb73bbc10c74334e7ad
x64v1openbsd=a0dcd7fa771891fee2cab6eecbb22ac2b94894d409697a24ac33f44a29c8538e
x64v1netbsd=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64v1musl=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64v1glibc=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64v1linux=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64v1elf=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
arm64mac=f0e34e5970415c0d376179e8dea931f9a183d7c2f0b8742b9479a7e82987bec0
arm64win=00b03a5f9c21f616abad611ed8755c45f7338556759adbfdb38c8ede7b0c61cb
arm64mingw=00b03a5f9c21f616abad611ed8755c45f7338556759adbfdb38c8ede7b0c61cb
arm64linux=f060db3aefea7bd9defa4e37f9fe8a1ee48f986bf2bec0a7a5f01f94c31ad38a
arm64musl=f060db3aefea7bd9defa4e37f9fe8a1ee48f986bf2bec0a7a5f01f94c31ad38a
arm64glibc=f060db3aefea7bd9defa4e37f9fe8a1ee48f986bf2bec0a7a5f01f94c31ad38a
arm64v1win=00b03a5f9c21f616abad611ed8755c45f7338556759adbfdb38c8ede7b0c61cb
arm64v1mingw=00b03a5f9c21f616abad611ed8755c45f7338556759adbfdb38c8ede7b0c61cb
arm64v1linux=f060db3aefea7bd9defa4e37f9fe8a1ee48f986bf2bec0a7a5f01f94c31ad38a
arm64v1musl=f060db3aefea7bd9defa4e37f9fe8a1ee48f986bf2bec0a7a5f01f94c31ad38a
arm64v1glibc=f060db3aefea7bd9defa4e37f9fe8a1ee48f986bf2bec0a7a5f01f94c31ad38a
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
