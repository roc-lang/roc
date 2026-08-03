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
    provides { "roc_main": main_for_host }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
    }

main_for_host : Str
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# app
main = "Hello, World!"

~~~
# DEV OUTPUT
~~~ini
x64mac=f4f75e48281443c0b5624020395df325a086132df4762e7bd4c2cb7d517dc8b0
x64win=acac8525063a17b607674d81cbef6e81425506e6406dfcead6636341a6054b7b
x64freebsd=a5d40824ca1897d83dc90dbca163e6b3ddbcdd325ead2843ecbfcd285cc7f70d
x64openbsd=99ae5608321a2a9b7e75a6ce3393d9f81f5242e1cd6dae6e9867ad6328fb90ff
x64netbsd=42769eb67f269459f422e1e3b050d95a16c7d7cd8558f3df514d3a42ff03a1c8
x64musl=42769eb67f269459f422e1e3b050d95a16c7d7cd8558f3df514d3a42ff03a1c8
x64glibc=42769eb67f269459f422e1e3b050d95a16c7d7cd8558f3df514d3a42ff03a1c8
x64linux=42769eb67f269459f422e1e3b050d95a16c7d7cd8558f3df514d3a42ff03a1c8
x64elf=42769eb67f269459f422e1e3b050d95a16c7d7cd8558f3df514d3a42ff03a1c8
x64v1mac=f4f75e48281443c0b5624020395df325a086132df4762e7bd4c2cb7d517dc8b0
x64v1win=acac8525063a17b607674d81cbef6e81425506e6406dfcead6636341a6054b7b
x64v1freebsd=42769eb67f269459f422e1e3b050d95a16c7d7cd8558f3df514d3a42ff03a1c8
x64v1openbsd=42769eb67f269459f422e1e3b050d95a16c7d7cd8558f3df514d3a42ff03a1c8
x64v1netbsd=42769eb67f269459f422e1e3b050d95a16c7d7cd8558f3df514d3a42ff03a1c8
x64v1musl=42769eb67f269459f422e1e3b050d95a16c7d7cd8558f3df514d3a42ff03a1c8
x64v1glibc=42769eb67f269459f422e1e3b050d95a16c7d7cd8558f3df514d3a42ff03a1c8
x64v1linux=42769eb67f269459f422e1e3b050d95a16c7d7cd8558f3df514d3a42ff03a1c8
x64v1elf=42769eb67f269459f422e1e3b050d95a16c7d7cd8558f3df514d3a42ff03a1c8
arm64mac=d833358b39e5a9edefc907fb32428a4e6b2497cb70cfa4521877d03eccce72e3
arm64win=4c2af047ef9db4c386bd4747c05082a91beb4dde12a256cce7399d976d4297a2
arm64linux=30b5774e5277393dbf2592116dd4318283eff8fd4f488383bea305c0b48d4bbb
arm64musl=30b5774e5277393dbf2592116dd4318283eff8fd4f488383bea305c0b48d4bbb
arm64glibc=30b5774e5277393dbf2592116dd4318283eff8fd4f488383bea305c0b48d4bbb
arm64v1win=4c2af047ef9db4c386bd4747c05082a91beb4dde12a256cce7399d976d4297a2
arm64v1linux=30b5774e5277393dbf2592116dd4318283eff8fd4f488383bea305c0b48d4bbb
arm64v1musl=30b5774e5277393dbf2592116dd4318283eff8fd4f488383bea305c0b48d4bbb
arm64v1glibc=30b5774e5277393dbf2592116dd4318283eff8fd4f488383bea305c0b48d4bbb
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
