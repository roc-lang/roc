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
x64mac=5c8631facb5df7f2637c4d976121812ca2aaeaaa67ed6045f7feade038d29d20
x64win=240636de2c08c3823e1b07e8430b847540e1ede493ee2cc9b6ddacc68f1e2019
x64mingw=240636de2c08c3823e1b07e8430b847540e1ede493ee2cc9b6ddacc68f1e2019
x64freebsd=268bf27fa5c42bbd9303c6dd4155877503f4102cd2514cb73bbc10c74334e7ad
x64openbsd=a0dcd7fa771891fee2cab6eecbb22ac2b94894d409697a24ac33f44a29c8538e
x64netbsd=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64musl=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64glibc=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64linux=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64elf=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64v1mac=5c8631facb5df7f2637c4d976121812ca2aaeaaa67ed6045f7feade038d29d20
x64v1win=240636de2c08c3823e1b07e8430b847540e1ede493ee2cc9b6ddacc68f1e2019
x64v1mingw=240636de2c08c3823e1b07e8430b847540e1ede493ee2cc9b6ddacc68f1e2019
x64v1freebsd=268bf27fa5c42bbd9303c6dd4155877503f4102cd2514cb73bbc10c74334e7ad
x64v1openbsd=a0dcd7fa771891fee2cab6eecbb22ac2b94894d409697a24ac33f44a29c8538e
x64v1netbsd=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64v1musl=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64v1glibc=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64v1linux=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
x64v1elf=4616d40f5094c77ad7094481314912515a15202b9aa038ae2e7137d2c9212d81
arm64mac=027b6607924e494c92389cf8ced47d970407a889ebfedbe087894657c2612937
arm64win=2492fc97bff183aabf8f2f735c848d51c4696541d536b0125b5a7dffc8ab0a57
arm64mingw=2492fc97bff183aabf8f2f735c848d51c4696541d536b0125b5a7dffc8ab0a57
arm64linux=f060db3aefea7bd9defa4e37f9fe8a1ee48f986bf2bec0a7a5f01f94c31ad38a
arm64musl=f060db3aefea7bd9defa4e37f9fe8a1ee48f986bf2bec0a7a5f01f94c31ad38a
arm64glibc=f060db3aefea7bd9defa4e37f9fe8a1ee48f986bf2bec0a7a5f01f94c31ad38a
arm64v1win=2492fc97bff183aabf8f2f735c848d51c4696541d536b0125b5a7dffc8ab0a57
arm64v1mingw=2492fc97bff183aabf8f2f735c848d51c4696541d536b0125b5a7dffc8ab0a57
arm64v1linux=f060db3aefea7bd9defa4e37f9fe8a1ee48f986bf2bec0a7a5f01f94c31ad38a
arm64v1musl=f060db3aefea7bd9defa4e37f9fe8a1ee48f986bf2bec0a7a5f01f94c31ad38a
arm64v1glibc=f060db3aefea7bd9defa4e37f9fe8a1ee48f986bf2bec0a7a5f01f94c31ad38a
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
