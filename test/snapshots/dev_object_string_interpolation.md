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
x64mac=8da26550b26e5db626e614569db98cfce6089627a349f5950a24720a30dc57b6
x64win=365b9a71bfe053847b3fe5a32e435465037e15e2b8685210bfc7799d040f0301
x64mingw=365b9a71bfe053847b3fe5a32e435465037e15e2b8685210bfc7799d040f0301
x64freebsd=0806c86318696ae4d8f98676825b6900bbe88163c7a52c1cbd24435eb876dc2a
x64openbsd=f7dae23b384af01fe7b9b4488190d316cfe8b059e0ed4501edd6d77ab7af344a
x64netbsd=4810307ab278f656d3534ae3e86c3dd7c8928c9c480b81d1543d53ceecc93ae3
x64musl=4810307ab278f656d3534ae3e86c3dd7c8928c9c480b81d1543d53ceecc93ae3
x64glibc=4810307ab278f656d3534ae3e86c3dd7c8928c9c480b81d1543d53ceecc93ae3
x64linux=4810307ab278f656d3534ae3e86c3dd7c8928c9c480b81d1543d53ceecc93ae3
x64elf=4810307ab278f656d3534ae3e86c3dd7c8928c9c480b81d1543d53ceecc93ae3
x64v1mac=8da26550b26e5db626e614569db98cfce6089627a349f5950a24720a30dc57b6
x64v1win=365b9a71bfe053847b3fe5a32e435465037e15e2b8685210bfc7799d040f0301
x64v1mingw=365b9a71bfe053847b3fe5a32e435465037e15e2b8685210bfc7799d040f0301
x64v1freebsd=0806c86318696ae4d8f98676825b6900bbe88163c7a52c1cbd24435eb876dc2a
x64v1openbsd=f7dae23b384af01fe7b9b4488190d316cfe8b059e0ed4501edd6d77ab7af344a
x64v1netbsd=4810307ab278f656d3534ae3e86c3dd7c8928c9c480b81d1543d53ceecc93ae3
x64v1musl=4810307ab278f656d3534ae3e86c3dd7c8928c9c480b81d1543d53ceecc93ae3
x64v1glibc=4810307ab278f656d3534ae3e86c3dd7c8928c9c480b81d1543d53ceecc93ae3
x64v1linux=4810307ab278f656d3534ae3e86c3dd7c8928c9c480b81d1543d53ceecc93ae3
x64v1elf=4810307ab278f656d3534ae3e86c3dd7c8928c9c480b81d1543d53ceecc93ae3
arm64mac=00d5cdb6c9e5675e6b3df54346909e4bdcb95d0b6ba32c44f6e741e7298bac66
arm64win=58114d72afce6ccb010d8f82bb3ebb59526019314136877d7e6cb0833fe3032f
arm64mingw=58114d72afce6ccb010d8f82bb3ebb59526019314136877d7e6cb0833fe3032f
arm64linux=9a6b74b5f102123002d8a4167c5d0524de342ace5b370e55b391ef245cd399e1
arm64musl=9a6b74b5f102123002d8a4167c5d0524de342ace5b370e55b391ef245cd399e1
arm64glibc=9a6b74b5f102123002d8a4167c5d0524de342ace5b370e55b391ef245cd399e1
arm64v1win=58114d72afce6ccb010d8f82bb3ebb59526019314136877d7e6cb0833fe3032f
arm64v1mingw=58114d72afce6ccb010d8f82bb3ebb59526019314136877d7e6cb0833fe3032f
arm64v1linux=9a6b74b5f102123002d8a4167c5d0524de342ace5b370e55b391ef245cd399e1
arm64v1musl=9a6b74b5f102123002d8a4167c5d0524de342ace5b370e55b391ef245cd399e1
arm64v1glibc=9a6b74b5f102123002d8a4167c5d0524de342ace5b370e55b391ef245cd399e1
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
