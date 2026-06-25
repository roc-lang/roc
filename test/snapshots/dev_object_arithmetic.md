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
x64mac=9ec801a0eb1bb1ae3a3ba849829168f5d1329c039bea5f75a2dd2a046b4e76b7
x64win=d286ad7c93561a310b64656d24d94ef6947b69f04b0fa9d5d9f9821561861022
x64freebsd=f08306ff97ce71ec5c0188823d5bd01251436fa57a593684f9ab87786ee88665
x64openbsd=f08306ff97ce71ec5c0188823d5bd01251436fa57a593684f9ab87786ee88665
x64netbsd=f08306ff97ce71ec5c0188823d5bd01251436fa57a593684f9ab87786ee88665
x64musl=f08306ff97ce71ec5c0188823d5bd01251436fa57a593684f9ab87786ee88665
x64glibc=f08306ff97ce71ec5c0188823d5bd01251436fa57a593684f9ab87786ee88665
x64linux=f08306ff97ce71ec5c0188823d5bd01251436fa57a593684f9ab87786ee88665
x64elf=f08306ff97ce71ec5c0188823d5bd01251436fa57a593684f9ab87786ee88665
arm64mac=59f610eabf9ea3009661b72477e58aae21bb5abe2fc5c99e5ee524151cfb1c42
arm64win=00b03a5f9c21f616abad611ed8755c45f7338556759adbfdb38c8ede7b0c61cb
arm64linux=07893f0e51b26324013322dfdd7d5c0a8297de2e4c05a895e606e022f54da78d
arm64musl=07893f0e51b26324013322dfdd7d5c0a8297de2e4c05a895e606e022f54da78d
arm64glibc=07893f0e51b26324013322dfdd7d5c0a8297de2e4c05a895e606e022f54da78d
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
