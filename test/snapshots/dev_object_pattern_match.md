# META
~~~ini
description=Tag unions and pattern matching
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

Color : [Red, Green, Blue]

to_str : Color -> Str
to_str = |color|
    match color {
        Red => "red"
        Green => "green"
        Blue => "blue"
    }

main = to_str(Red)
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
to_str = |color| match color {
	Red => "red"
	Green => "green"
	Blue => "blue"
}
main = to_str(Red)

~~~
# DEV OUTPUT
~~~ini
x64mac=37519ad43de3bdb36bdcbae8faa36348d3fbe156cef9013c7bfb0ef9003ecf4f
x64win=2fdeb6618f46323fb744e334b8626aa39c7a0789547604a2551fad71e67ac628
x64mingw=2fdeb6618f46323fb744e334b8626aa39c7a0789547604a2551fad71e67ac628
x64freebsd=39702334debca5675bc03662892651e48db5bb63eca713797f3f3ff1da2d7856
x64openbsd=f6f841a85868a18ad4edb3d4e34326c2f18cab7eb7a0142de2316b233e810eca
x64netbsd=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64musl=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64glibc=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64linux=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64elf=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64v1mac=37519ad43de3bdb36bdcbae8faa36348d3fbe156cef9013c7bfb0ef9003ecf4f
x64v1win=2fdeb6618f46323fb744e334b8626aa39c7a0789547604a2551fad71e67ac628
x64v1mingw=2fdeb6618f46323fb744e334b8626aa39c7a0789547604a2551fad71e67ac628
x64v1freebsd=39702334debca5675bc03662892651e48db5bb63eca713797f3f3ff1da2d7856
x64v1openbsd=f6f841a85868a18ad4edb3d4e34326c2f18cab7eb7a0142de2316b233e810eca
x64v1netbsd=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64v1musl=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64v1glibc=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64v1linux=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64v1elf=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
arm64mac=18b68ab744a90923df1cfa66ba65d88c832b344ec78e5af01b1cd48f2e147c14
arm64win=60858971cb13db2c6d74e6c7f5260e4b49e6cd82af9548286a1d5c19e1000b0c
arm64mingw=60858971cb13db2c6d74e6c7f5260e4b49e6cd82af9548286a1d5c19e1000b0c
arm64linux=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm64musl=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm64glibc=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm64v1win=60858971cb13db2c6d74e6c7f5260e4b49e6cd82af9548286a1d5c19e1000b0c
arm64v1mingw=60858971cb13db2c6d74e6c7f5260e4b49e6cd82af9548286a1d5c19e1000b0c
arm64v1linux=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm64v1musl=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm64v1glibc=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
