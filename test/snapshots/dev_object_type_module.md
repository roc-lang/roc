# META
~~~ini
description=Type mod import with multi-mod compilation
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

import Color

main = Color.to_str(Color.red({}))
~~~
## Color.roc
~~~roc
Color := [Red, Green, Blue].{
    red : {} -> Color
    red = |{}| Red

    green : {} -> Color
    green = |{}| Green

    blue : {} -> Color
    blue = |{}| Blue

    to_str : Color -> Str
    to_str = |color|
        match color {
            Red => "red"
            Green => "green"
            Blue => "blue"
        }
}
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

# Color
Color.red = |{}| Red
Color.green = |{}| Green
Color.blue = |{}| Blue
Color.to_str = |color| match color {
	Red => "red"
	Green => "green"
	Blue => "blue"
}

# app
main = to_str(red({}))

~~~
# DEV OUTPUT
~~~ini
x64mac=702595f983458b3df0d3cf7279d63df01dee035dc8eb492273ee891adfc6c139
x64win=0f0ec8aeaefe9fd6cc284d3c369e4d67253166ec7f0f9e3abd2e05429768a9c6
x64mingw=0f0ec8aeaefe9fd6cc284d3c369e4d67253166ec7f0f9e3abd2e05429768a9c6
x64freebsd=39702334debca5675bc03662892651e48db5bb63eca713797f3f3ff1da2d7856
x64openbsd=f6f841a85868a18ad4edb3d4e34326c2f18cab7eb7a0142de2316b233e810eca
x64netbsd=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64musl=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64glibc=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64linux=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64elf=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64v1mac=702595f983458b3df0d3cf7279d63df01dee035dc8eb492273ee891adfc6c139
x64v1win=0f0ec8aeaefe9fd6cc284d3c369e4d67253166ec7f0f9e3abd2e05429768a9c6
x64v1mingw=0f0ec8aeaefe9fd6cc284d3c369e4d67253166ec7f0f9e3abd2e05429768a9c6
x64v1freebsd=39702334debca5675bc03662892651e48db5bb63eca713797f3f3ff1da2d7856
x64v1openbsd=f6f841a85868a18ad4edb3d4e34326c2f18cab7eb7a0142de2316b233e810eca
x64v1netbsd=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64v1musl=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64v1glibc=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64v1linux=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64v1elf=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
arm64mac=5b3b80bc646dc2d898fc3e8b80d0973bd230de9473c596af71dd5c9136432833
arm64win=f66be229ca2640147871869d8fa49b18c46a4b3b2e683d70864f2889a6141601
arm64mingw=f66be229ca2640147871869d8fa49b18c46a4b3b2e683d70864f2889a6141601
arm64linux=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm64musl=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm64glibc=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm64v1win=f66be229ca2640147871869d8fa49b18c46a4b3b2e683d70864f2889a6141601
arm64v1mingw=f66be229ca2640147871869d8fa49b18c46a4b3b2e683d70864f2889a6141601
arm64v1linux=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm64v1musl=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm64v1glibc=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
