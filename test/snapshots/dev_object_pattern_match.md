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
x64mac=4dbcb7a4211ff786eae01faa50b443c54da00f25e99949cc2f93462e604ca906
x64win=9dbf34632054071d55df088e589d3ed2ec81953197a30949da24db5dd31f0a0a
x64mingw=9dbf34632054071d55df088e589d3ed2ec81953197a30949da24db5dd31f0a0a
x64freebsd=4173feebcb6ef26940fa9417dfc057cfb396cd05e0eaa8e2afd1f68288b07e07
x64openbsd=a6416b7d5f143dccccdab65dd96700f43c367d4ad279e7308431ceac2d91a12e
x64netbsd=99529a3071d169bf85767bef7c7c1dbeb8d697e059cebbb535f0577ea1689bf0
x64musl=99529a3071d169bf85767bef7c7c1dbeb8d697e059cebbb535f0577ea1689bf0
x64glibc=99529a3071d169bf85767bef7c7c1dbeb8d697e059cebbb535f0577ea1689bf0
x64linux=99529a3071d169bf85767bef7c7c1dbeb8d697e059cebbb535f0577ea1689bf0
x64elf=99529a3071d169bf85767bef7c7c1dbeb8d697e059cebbb535f0577ea1689bf0
x64v1mac=4dbcb7a4211ff786eae01faa50b443c54da00f25e99949cc2f93462e604ca906
x64v1win=9dbf34632054071d55df088e589d3ed2ec81953197a30949da24db5dd31f0a0a
x64v1mingw=9dbf34632054071d55df088e589d3ed2ec81953197a30949da24db5dd31f0a0a
x64v1freebsd=4173feebcb6ef26940fa9417dfc057cfb396cd05e0eaa8e2afd1f68288b07e07
x64v1openbsd=a6416b7d5f143dccccdab65dd96700f43c367d4ad279e7308431ceac2d91a12e
x64v1netbsd=99529a3071d169bf85767bef7c7c1dbeb8d697e059cebbb535f0577ea1689bf0
x64v1musl=99529a3071d169bf85767bef7c7c1dbeb8d697e059cebbb535f0577ea1689bf0
x64v1glibc=99529a3071d169bf85767bef7c7c1dbeb8d697e059cebbb535f0577ea1689bf0
x64v1linux=99529a3071d169bf85767bef7c7c1dbeb8d697e059cebbb535f0577ea1689bf0
x64v1elf=99529a3071d169bf85767bef7c7c1dbeb8d697e059cebbb535f0577ea1689bf0
arm64mac=4a362219665ee19d7cf0640c3bc3276a05a2a4c6230a2f077d654a935bade704
arm64win=ed2b4db9716e572bd340be14426ecf03ae2c383effd3b5b064544b4dfe963170
arm64mingw=ed2b4db9716e572bd340be14426ecf03ae2c383effd3b5b064544b4dfe963170
arm64linux=9a252e89e96c0f17b34e952aa8bebde68ef925ad3c2179d24e5e99f07a546d4b
arm64musl=9a252e89e96c0f17b34e952aa8bebde68ef925ad3c2179d24e5e99f07a546d4b
arm64glibc=9a252e89e96c0f17b34e952aa8bebde68ef925ad3c2179d24e5e99f07a546d4b
arm64v1win=ed2b4db9716e572bd340be14426ecf03ae2c383effd3b5b064544b4dfe963170
arm64v1mingw=ed2b4db9716e572bd340be14426ecf03ae2c383effd3b5b064544b4dfe963170
arm64v1linux=9a252e89e96c0f17b34e952aa8bebde68ef925ad3c2179d24e5e99f07a546d4b
arm64v1musl=9a252e89e96c0f17b34e952aa8bebde68ef925ad3c2179d24e5e99f07a546d4b
arm64v1glibc=9a252e89e96c0f17b34e952aa8bebde68ef925ad3c2179d24e5e99f07a546d4b
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
