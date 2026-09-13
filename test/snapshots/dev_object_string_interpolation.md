# META
~~~ini
description=String interpolation and concatenation
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

greeting = "Hello"
name = "World"
main = "${greeting}, ${name}!"
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
greeting = "Hello"
name = "World"
main = {
	cinterp_0 = greeting
	cinterp_1 = name
	<interpolation>("", [cinterp_0, ", ", cinterp_1, "!"])
}

~~~
# DEV OUTPUT
~~~ini
x64mac=b6dafa505b2b6e7b74ccbfdc99cd8ec782b0428f5720c09ac14cdda5935189da
x64win=30f701f7351cfd662b60af5b4024073e963075e6c3dfdfa19e176a77b8e29964
x64mingw=30f701f7351cfd662b60af5b4024073e963075e6c3dfdfa19e176a77b8e29964
x64freebsd=8aeaad2873fb8bb57df1ae44272b481b3a1332ec95c27f398ead2375cc566420
x64openbsd=62dcd954569f561238a0024652c55a81f8802c396e988d2af99a0d7812123c0a
x64netbsd=836ddbe1d630c12b123148ecceba36db6260ed556c0ee2e7595dc7d81c23a5a3
x64musl=836ddbe1d630c12b123148ecceba36db6260ed556c0ee2e7595dc7d81c23a5a3
x64glibc=836ddbe1d630c12b123148ecceba36db6260ed556c0ee2e7595dc7d81c23a5a3
x64linux=836ddbe1d630c12b123148ecceba36db6260ed556c0ee2e7595dc7d81c23a5a3
x64elf=836ddbe1d630c12b123148ecceba36db6260ed556c0ee2e7595dc7d81c23a5a3
x64v1mac=b6dafa505b2b6e7b74ccbfdc99cd8ec782b0428f5720c09ac14cdda5935189da
x64v1win=30f701f7351cfd662b60af5b4024073e963075e6c3dfdfa19e176a77b8e29964
x64v1mingw=30f701f7351cfd662b60af5b4024073e963075e6c3dfdfa19e176a77b8e29964
x64v1freebsd=8aeaad2873fb8bb57df1ae44272b481b3a1332ec95c27f398ead2375cc566420
x64v1openbsd=62dcd954569f561238a0024652c55a81f8802c396e988d2af99a0d7812123c0a
x64v1netbsd=836ddbe1d630c12b123148ecceba36db6260ed556c0ee2e7595dc7d81c23a5a3
x64v1musl=836ddbe1d630c12b123148ecceba36db6260ed556c0ee2e7595dc7d81c23a5a3
x64v1glibc=836ddbe1d630c12b123148ecceba36db6260ed556c0ee2e7595dc7d81c23a5a3
x64v1linux=836ddbe1d630c12b123148ecceba36db6260ed556c0ee2e7595dc7d81c23a5a3
x64v1elf=836ddbe1d630c12b123148ecceba36db6260ed556c0ee2e7595dc7d81c23a5a3
arm64mac=8548dec771a3c030806974591b1f335db7de86311a40c6ad50ab9aeaf0ce0baa
arm64win=faef29c6cf21c25a04c97740ff333b08c3154667ef19b6656fc106821512b97d
arm64mingw=faef29c6cf21c25a04c97740ff333b08c3154667ef19b6656fc106821512b97d
arm64linux=9ca898878c400a1ea070e88f2fc4b0b481f74e22cfd95aebd289d8c08dab4ebe
arm64musl=9ca898878c400a1ea070e88f2fc4b0b481f74e22cfd95aebd289d8c08dab4ebe
arm64glibc=9ca898878c400a1ea070e88f2fc4b0b481f74e22cfd95aebd289d8c08dab4ebe
arm64v1win=faef29c6cf21c25a04c97740ff333b08c3154667ef19b6656fc106821512b97d
arm64v1mingw=faef29c6cf21c25a04c97740ff333b08c3154667ef19b6656fc106821512b97d
arm64v1linux=9ca898878c400a1ea070e88f2fc4b0b481f74e22cfd95aebd289d8c08dab4ebe
arm64v1musl=9ca898878c400a1ea070e88f2fc4b0b481f74e22cfd95aebd289d8c08dab4ebe
arm64v1glibc=9ca898878c400a1ea070e88f2fc4b0b481f74e22cfd95aebd289d8c08dab4ebe
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
