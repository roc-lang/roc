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
        inputs: "targets/",
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
main = add(3, 4).times(2)
add = |a, b| a.plus(b)

~~~
# DEV OUTPUT
~~~ini
x64mac=a35fc7e596f7d1e9f41449af5f7409d92cc23bcd60202c029a226eec462cbc0b
x64win=72862574fdacefbcd10ee31b471295b24c7f46242ec9e44cab4375cc5172b18b
x64freebsd=ccc64dfa50cbefe6d81b08996e34e72ccc52bd592288d584d25d99841f3f591a
x64openbsd=ccc64dfa50cbefe6d81b08996e34e72ccc52bd592288d584d25d99841f3f591a
x64netbsd=ccc64dfa50cbefe6d81b08996e34e72ccc52bd592288d584d25d99841f3f591a
x64musl=ccc64dfa50cbefe6d81b08996e34e72ccc52bd592288d584d25d99841f3f591a
x64glibc=ccc64dfa50cbefe6d81b08996e34e72ccc52bd592288d584d25d99841f3f591a
x64linux=ccc64dfa50cbefe6d81b08996e34e72ccc52bd592288d584d25d99841f3f591a
x64elf=ccc64dfa50cbefe6d81b08996e34e72ccc52bd592288d584d25d99841f3f591a
arm64mac=d47a4f687283f3de7ad38a061f44499e9a50a1e90904aa1203356736c131935e
arm64win=ac7c5a8bcfcc587fa17a37085c2b3388b9b034c670a12de1b81f3a40a6bc3f79
arm64linux=c1a7868f202b9fadd9710b083824f4bb9264141916c8946e4acb7c3e8c37c320
arm64musl=c1a7868f202b9fadd9710b083824f4bb9264141916c8946e4acb7c3e8c37c320
arm64glibc=c1a7868f202b9fadd9710b083824f4bb9264141916c8946e4acb7c3e8c37c320
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
