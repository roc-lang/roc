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
x64mac=83e403227f3a3a89aa26fdf9a754f0fb242672119532d253902a21e1d8dd6651
x64win=2b89c6eb0f5c019971ce378a9630183eabb1d5dbc8d023f13b57e46b7fff6601
x64mingw=2b89c6eb0f5c019971ce378a9630183eabb1d5dbc8d023f13b57e46b7fff6601
x64freebsd=01c4e5d24da0d61303353a6343a69470c0524f2cc709e0f1c97963ae2fccdf6e
x64openbsd=850ed4f77734b8df4bdba1753736696d06bef2e7b3ed4bb3956eb604f936a059
x64netbsd=f5efc4314a9cf21336b5258e9aa23dc4272afa5cc1621cac8b7ef2da3c92576b
x64musl=f5efc4314a9cf21336b5258e9aa23dc4272afa5cc1621cac8b7ef2da3c92576b
x64glibc=f5efc4314a9cf21336b5258e9aa23dc4272afa5cc1621cac8b7ef2da3c92576b
x64linux=f5efc4314a9cf21336b5258e9aa23dc4272afa5cc1621cac8b7ef2da3c92576b
x64elf=f5efc4314a9cf21336b5258e9aa23dc4272afa5cc1621cac8b7ef2da3c92576b
x64v1mac=83e403227f3a3a89aa26fdf9a754f0fb242672119532d253902a21e1d8dd6651
x64v1win=2b89c6eb0f5c019971ce378a9630183eabb1d5dbc8d023f13b57e46b7fff6601
x64v1mingw=2b89c6eb0f5c019971ce378a9630183eabb1d5dbc8d023f13b57e46b7fff6601
x64v1freebsd=01c4e5d24da0d61303353a6343a69470c0524f2cc709e0f1c97963ae2fccdf6e
x64v1openbsd=850ed4f77734b8df4bdba1753736696d06bef2e7b3ed4bb3956eb604f936a059
x64v1netbsd=f5efc4314a9cf21336b5258e9aa23dc4272afa5cc1621cac8b7ef2da3c92576b
x64v1musl=f5efc4314a9cf21336b5258e9aa23dc4272afa5cc1621cac8b7ef2da3c92576b
x64v1glibc=f5efc4314a9cf21336b5258e9aa23dc4272afa5cc1621cac8b7ef2da3c92576b
x64v1linux=f5efc4314a9cf21336b5258e9aa23dc4272afa5cc1621cac8b7ef2da3c92576b
x64v1elf=f5efc4314a9cf21336b5258e9aa23dc4272afa5cc1621cac8b7ef2da3c92576b
arm64mac=45113874f01de7261b50c09ac9012dc0f869f4b5bbe480eede2dbab3ca439155
arm64win=86f8db7746f10e03adbcd62c240a1806ecd05f9e36869e59d3bec231171a446d
arm64mingw=86f8db7746f10e03adbcd62c240a1806ecd05f9e36869e59d3bec231171a446d
arm64linux=74c61e13cdd72b05312e84a6bec88adffb344c0d03c912c920c3eef6bdfc0e15
arm64musl=74c61e13cdd72b05312e84a6bec88adffb344c0d03c912c920c3eef6bdfc0e15
arm64glibc=74c61e13cdd72b05312e84a6bec88adffb344c0d03c912c920c3eef6bdfc0e15
arm64v1win=86f8db7746f10e03adbcd62c240a1806ecd05f9e36869e59d3bec231171a446d
arm64v1mingw=86f8db7746f10e03adbcd62c240a1806ecd05f9e36869e59d3bec231171a446d
arm64v1linux=74c61e13cdd72b05312e84a6bec88adffb344c0d03c912c920c3eef6bdfc0e15
arm64v1musl=74c61e13cdd72b05312e84a6bec88adffb344c0d03c912c920c3eef6bdfc0e15
arm64v1glibc=74c61e13cdd72b05312e84a6bec88adffb344c0d03c912c920c3eef6bdfc0e15
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
