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
x64mac=702595f983458b3df0d3cf7279d63df01dee035dc8eb492273ee891adfc6c139
x64win=16701839dcd473dd36cfd7780fa18a80af4ff6d142b4e624a172ad72fbba02ff
x64mingw=16701839dcd473dd36cfd7780fa18a80af4ff6d142b4e624a172ad72fbba02ff
x64freebsd=39702334debca5675bc03662892651e48db5bb63eca713797f3f3ff1da2d7856
x64openbsd=f6f841a85868a18ad4edb3d4e34326c2f18cab7eb7a0142de2316b233e810eca
x64netbsd=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64musl=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64glibc=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64linux=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64elf=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64v1mac=702595f983458b3df0d3cf7279d63df01dee035dc8eb492273ee891adfc6c139
x64v1win=16701839dcd473dd36cfd7780fa18a80af4ff6d142b4e624a172ad72fbba02ff
x64v1mingw=16701839dcd473dd36cfd7780fa18a80af4ff6d142b4e624a172ad72fbba02ff
x64v1freebsd=39702334debca5675bc03662892651e48db5bb63eca713797f3f3ff1da2d7856
x64v1openbsd=f6f841a85868a18ad4edb3d4e34326c2f18cab7eb7a0142de2316b233e810eca
x64v1netbsd=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64v1musl=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64v1glibc=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64v1linux=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
x64v1elf=3065425feb8057256f25caaf0495fbf0e359b9beb04564ad5805cc93970bff8b
arm64mac=5b3b80bc646dc2d898fc3e8b80d0973bd230de9473c596af71dd5c9136432833
arm64win=932711c1ff9614ea2b60ad62b6a75c8b9973b23938f88a89225e783c180c667a
arm64mingw=932711c1ff9614ea2b60ad62b6a75c8b9973b23938f88a89225e783c180c667a
arm64linux=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm64musl=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm64glibc=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm64v1win=932711c1ff9614ea2b60ad62b6a75c8b9973b23938f88a89225e783c180c667a
arm64v1mingw=932711c1ff9614ea2b60ad62b6a75c8b9973b23938f88a89225e783c180c667a
arm64v1linux=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm64v1musl=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm64v1glibc=08360fb7a65646d2bb435099dce112cfa6d240673e86f983a7948ed6d24a432a
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
