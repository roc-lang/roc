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
x64mac=2937f1ff75e06066dd93e29df544814ed4a983da7a1fa80b3c9bb8ebead1c4b3
x64win=b146b852790152422385e1a181164e6e5aa42012f3bbfaf77708ffcaad18d461
x64mingw=b146b852790152422385e1a181164e6e5aa42012f3bbfaf77708ffcaad18d461
x64freebsd=99a5d6093e7162d85374a999c80f819806e904d3d9927e8ee410377593baba6d
x64openbsd=184654b40d22e17865c15c71923da2ab3960ee08e167c40ea0441a5d32d89597
x64netbsd=7839de9d0fbfacf1b71b90282014f46a58b8ecd9b0dcc7798427d5277a6d55c7
x64musl=7839de9d0fbfacf1b71b90282014f46a58b8ecd9b0dcc7798427d5277a6d55c7
x64glibc=7839de9d0fbfacf1b71b90282014f46a58b8ecd9b0dcc7798427d5277a6d55c7
x64linux=7839de9d0fbfacf1b71b90282014f46a58b8ecd9b0dcc7798427d5277a6d55c7
x64elf=7839de9d0fbfacf1b71b90282014f46a58b8ecd9b0dcc7798427d5277a6d55c7
x64v1mac=2937f1ff75e06066dd93e29df544814ed4a983da7a1fa80b3c9bb8ebead1c4b3
x64v1win=b146b852790152422385e1a181164e6e5aa42012f3bbfaf77708ffcaad18d461
x64v1mingw=b146b852790152422385e1a181164e6e5aa42012f3bbfaf77708ffcaad18d461
x64v1freebsd=99a5d6093e7162d85374a999c80f819806e904d3d9927e8ee410377593baba6d
x64v1openbsd=184654b40d22e17865c15c71923da2ab3960ee08e167c40ea0441a5d32d89597
x64v1netbsd=7839de9d0fbfacf1b71b90282014f46a58b8ecd9b0dcc7798427d5277a6d55c7
x64v1musl=7839de9d0fbfacf1b71b90282014f46a58b8ecd9b0dcc7798427d5277a6d55c7
x64v1glibc=7839de9d0fbfacf1b71b90282014f46a58b8ecd9b0dcc7798427d5277a6d55c7
x64v1linux=7839de9d0fbfacf1b71b90282014f46a58b8ecd9b0dcc7798427d5277a6d55c7
x64v1elf=7839de9d0fbfacf1b71b90282014f46a58b8ecd9b0dcc7798427d5277a6d55c7
arm64mac=8871d551fd1cc4a4e0a7e2e898ab7f9dcd06ccfda8674db22f23303cd9deaa9b
arm64win=b8bb21ce3ea8ba14ae074eb3e868fa4575b31ae57423c7e936bc9f105d749287
arm64mingw=b8bb21ce3ea8ba14ae074eb3e868fa4575b31ae57423c7e936bc9f105d749287
arm64linux=0cd555c83957678252c110802df765f49e06c62e935621f55236feac8217ec8c
arm64musl=0cd555c83957678252c110802df765f49e06c62e935621f55236feac8217ec8c
arm64glibc=0cd555c83957678252c110802df765f49e06c62e935621f55236feac8217ec8c
arm64v1win=b8bb21ce3ea8ba14ae074eb3e868fa4575b31ae57423c7e936bc9f105d749287
arm64v1mingw=b8bb21ce3ea8ba14ae074eb3e868fa4575b31ae57423c7e936bc9f105d749287
arm64v1linux=0cd555c83957678252c110802df765f49e06c62e935621f55236feac8217ec8c
arm64v1musl=0cd555c83957678252c110802df765f49e06c62e935621f55236feac8217ec8c
arm64v1glibc=0cd555c83957678252c110802df765f49e06c62e935621f55236feac8217ec8c
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
