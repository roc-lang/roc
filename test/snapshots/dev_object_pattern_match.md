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
x64mac=2d31d212e0bbd77dc5f00a2131ad6b74a277ae6fb8cfc4cab48725c0d3bc9f9f
x64win=9dbf34632054071d55df088e589d3ed2ec81953197a30949da24db5dd31f0a0a
x64mingw=9dbf34632054071d55df088e589d3ed2ec81953197a30949da24db5dd31f0a0a
x64freebsd=ccb9e4b22f05df9ef9a54742e4ef6da4d266f85334f73c099372eb49043a50d6
x64openbsd=dfeacefc02ff7c4037fa93a1778521484a093af98b5c252309eb434f66f943d5
x64netbsd=d417a045378026cb2d22667da5046824d7467256d0fec98efff5d542dd9c1451
x64musl=d417a045378026cb2d22667da5046824d7467256d0fec98efff5d542dd9c1451
x64glibc=d417a045378026cb2d22667da5046824d7467256d0fec98efff5d542dd9c1451
x64linux=d417a045378026cb2d22667da5046824d7467256d0fec98efff5d542dd9c1451
x64elf=d417a045378026cb2d22667da5046824d7467256d0fec98efff5d542dd9c1451
x64v1mac=2d31d212e0bbd77dc5f00a2131ad6b74a277ae6fb8cfc4cab48725c0d3bc9f9f
x64v1win=9dbf34632054071d55df088e589d3ed2ec81953197a30949da24db5dd31f0a0a
x64v1mingw=9dbf34632054071d55df088e589d3ed2ec81953197a30949da24db5dd31f0a0a
x64v1freebsd=ccb9e4b22f05df9ef9a54742e4ef6da4d266f85334f73c099372eb49043a50d6
x64v1openbsd=dfeacefc02ff7c4037fa93a1778521484a093af98b5c252309eb434f66f943d5
x64v1netbsd=d417a045378026cb2d22667da5046824d7467256d0fec98efff5d542dd9c1451
x64v1musl=d417a045378026cb2d22667da5046824d7467256d0fec98efff5d542dd9c1451
x64v1glibc=d417a045378026cb2d22667da5046824d7467256d0fec98efff5d542dd9c1451
x64v1linux=d417a045378026cb2d22667da5046824d7467256d0fec98efff5d542dd9c1451
x64v1elf=d417a045378026cb2d22667da5046824d7467256d0fec98efff5d542dd9c1451
arm64mac=6c8406d3586877dd4e2a810eb1dfb3b4cb0fc038ee0dc414e96039de53eac456
arm64win=ed2b4db9716e572bd340be14426ecf03ae2c383effd3b5b064544b4dfe963170
arm64mingw=ed2b4db9716e572bd340be14426ecf03ae2c383effd3b5b064544b4dfe963170
arm64linux=3df790a922d96fbfb0598573019c0e3f73ac4b6deb97eb27e6e2acc6794c95d5
arm64musl=3df790a922d96fbfb0598573019c0e3f73ac4b6deb97eb27e6e2acc6794c95d5
arm64glibc=3df790a922d96fbfb0598573019c0e3f73ac4b6deb97eb27e6e2acc6794c95d5
arm64v1win=ed2b4db9716e572bd340be14426ecf03ae2c383effd3b5b064544b4dfe963170
arm64v1mingw=ed2b4db9716e572bd340be14426ecf03ae2c383effd3b5b064544b4dfe963170
arm64v1linux=3df790a922d96fbfb0598573019c0e3f73ac4b6deb97eb27e6e2acc6794c95d5
arm64v1musl=3df790a922d96fbfb0598573019c0e3f73ac4b6deb97eb27e6e2acc6794c95d5
arm64v1glibc=3df790a922d96fbfb0598573019c0e3f73ac4b6deb97eb27e6e2acc6794c95d5
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
