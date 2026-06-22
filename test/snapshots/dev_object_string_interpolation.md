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
x64mac=87e88a1d400dc6fe83da9f6e73115c1f68fed94d5b7bfd883960eed60d8086e0
x64win=8d4a4dc0ce2a4a812bfea9b94eb605c8731f7ee23786884214c86859b5ea4152
x64freebsd=28f1e211670c2245d65473b697f4cfac4b7f76f75891dbabb4755ccdfc7c42ee
x64openbsd=28f1e211670c2245d65473b697f4cfac4b7f76f75891dbabb4755ccdfc7c42ee
x64netbsd=28f1e211670c2245d65473b697f4cfac4b7f76f75891dbabb4755ccdfc7c42ee
x64musl=28f1e211670c2245d65473b697f4cfac4b7f76f75891dbabb4755ccdfc7c42ee
x64glibc=28f1e211670c2245d65473b697f4cfac4b7f76f75891dbabb4755ccdfc7c42ee
x64linux=28f1e211670c2245d65473b697f4cfac4b7f76f75891dbabb4755ccdfc7c42ee
x64elf=28f1e211670c2245d65473b697f4cfac4b7f76f75891dbabb4755ccdfc7c42ee
arm64mac=c478d2ee3e7f86769a586b98a3e4b2a073733e60fc683972e216625e889717bc
arm64win=88d22672812a1a8b92d1f2cde697d98c61c8c8bd04807c28447d01637c4dbace
arm64linux=29d8e56ecf9b353fee815c57cc2978b19344597d2d8a182a95ec61b99fbec212
arm64musl=29d8e56ecf9b353fee815c57cc2978b19344597d2d8a182a95ec61b99fbec212
arm64glibc=29d8e56ecf9b353fee815c57cc2978b19344597d2d8a182a95ec61b99fbec212
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
