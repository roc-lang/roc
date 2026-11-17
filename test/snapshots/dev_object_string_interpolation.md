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
x64mac=5d41a0634019c8965a71ddcfd0dc02d0b20c179b54a6c7f9ff6fc29fefe0a558
x64win=1575c4cb38d2aa6d7570c42896cae8c9578ad018d559442bd8da84a85d0561f7
x64mingw=1575c4cb38d2aa6d7570c42896cae8c9578ad018d559442bd8da84a85d0561f7
x64freebsd=2eea130d07563730d35b0c0cabcb7008428cb833d4c77fb15875485ff25de627
x64openbsd=4fceb49982793e8a9b4fab3397d50726a77dc309e8f916c3215efb03a598d351
x64netbsd=214285c6a45926fdafedb509bb054e6603f1a356c1a2e84c0a4b8ab2d18e6b55
x64musl=214285c6a45926fdafedb509bb054e6603f1a356c1a2e84c0a4b8ab2d18e6b55
x64glibc=214285c6a45926fdafedb509bb054e6603f1a356c1a2e84c0a4b8ab2d18e6b55
x64linux=214285c6a45926fdafedb509bb054e6603f1a356c1a2e84c0a4b8ab2d18e6b55
x64elf=214285c6a45926fdafedb509bb054e6603f1a356c1a2e84c0a4b8ab2d18e6b55
x64v1mac=5d41a0634019c8965a71ddcfd0dc02d0b20c179b54a6c7f9ff6fc29fefe0a558
x64v1win=1575c4cb38d2aa6d7570c42896cae8c9578ad018d559442bd8da84a85d0561f7
x64v1mingw=1575c4cb38d2aa6d7570c42896cae8c9578ad018d559442bd8da84a85d0561f7
x64v1freebsd=2eea130d07563730d35b0c0cabcb7008428cb833d4c77fb15875485ff25de627
x64v1openbsd=4fceb49982793e8a9b4fab3397d50726a77dc309e8f916c3215efb03a598d351
x64v1netbsd=214285c6a45926fdafedb509bb054e6603f1a356c1a2e84c0a4b8ab2d18e6b55
x64v1musl=214285c6a45926fdafedb509bb054e6603f1a356c1a2e84c0a4b8ab2d18e6b55
x64v1glibc=214285c6a45926fdafedb509bb054e6603f1a356c1a2e84c0a4b8ab2d18e6b55
x64v1linux=214285c6a45926fdafedb509bb054e6603f1a356c1a2e84c0a4b8ab2d18e6b55
x64v1elf=214285c6a45926fdafedb509bb054e6603f1a356c1a2e84c0a4b8ab2d18e6b55
arm64mac=f6daad5d48700ab2beb06e7652bee03ce1ba9a1748d5a2fe752be4050f959d8b
arm64win=ccaa8f0b05520fb92e891e98c15fa2623e8c2e668ccbcf0ecd999b83bcafe83c
arm64mingw=ccaa8f0b05520fb92e891e98c15fa2623e8c2e668ccbcf0ecd999b83bcafe83c
arm64linux=d3ad1bcd0a9cdf95e62a6482ec30bdebadf7029ff4d09ce0e88588ef9732271f
arm64musl=d3ad1bcd0a9cdf95e62a6482ec30bdebadf7029ff4d09ce0e88588ef9732271f
arm64glibc=d3ad1bcd0a9cdf95e62a6482ec30bdebadf7029ff4d09ce0e88588ef9732271f
arm64v1win=ccaa8f0b05520fb92e891e98c15fa2623e8c2e668ccbcf0ecd999b83bcafe83c
arm64v1mingw=ccaa8f0b05520fb92e891e98c15fa2623e8c2e668ccbcf0ecd999b83bcafe83c
arm64v1linux=d3ad1bcd0a9cdf95e62a6482ec30bdebadf7029ff4d09ce0e88588ef9732271f
arm64v1musl=d3ad1bcd0a9cdf95e62a6482ec30bdebadf7029ff4d09ce0e88588ef9732271f
arm64v1glibc=d3ad1bcd0a9cdf95e62a6482ec30bdebadf7029ff4d09ce0e88588ef9732271f
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
