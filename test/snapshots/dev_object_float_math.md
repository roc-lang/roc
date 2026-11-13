# META
~~~ini
description=F32/F64 arithmetic, comparisons, and F64 to I64 conversion in host-called functions
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

main : F64, F32, I64 -> I64
main = |x, y, n| {
    scaled = x * 2.5 - I64.to_f64(n) / 3.0
    narrowed = F32.to_f64(y * 1.5 + 0.25)
    total = if scaled > narrowed { scaled - narrowed } else { narrowed + scaled }
    F64.to_i64_wrap(total)
}
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : F64, F32, I64 -> I64 }
    exposes []
    packages {}
    provides { "roc_main": main_for_host }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
    }

main_for_host : F64, F32, I64 -> I64
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# app
main = |x, y, n| {
	scaled = x * 2.5 - to_f64(n) / 3
	narrowed = to_f64(y * 1.5 + 0.25)
	total = if (scaled > narrowed) {
		scaled - narrowed
	} else {
		narrowed + scaled
	}
	to_i64_wrap(total)
}

~~~
# DEV OUTPUT
~~~ini
x64mac=7c58e2712841fe9eed7cc27a6e428189c4d7f290b000220bacb3edf75635ee32
x64win=eab23aaddafaa0228e879af809afd16370ece3fcc579fe5ab184602a62a6ab4c
x64mingw=eab23aaddafaa0228e879af809afd16370ece3fcc579fe5ab184602a62a6ab4c
x64freebsd=4ed981504e0ee18949d5902f5844c9f3c818e69f461bdd31b7d799f7add62b15
x64openbsd=42e3f7a307b47185bed59dc7864d9cb6f76f08d1ff3f13001c130c734f83c346
x64netbsd=4c38d2a841e201fcb49c4e0c1d80e6d19d0685bf617b43b07929f5c5bd0e33bf
x64musl=4c38d2a841e201fcb49c4e0c1d80e6d19d0685bf617b43b07929f5c5bd0e33bf
x64glibc=4c38d2a841e201fcb49c4e0c1d80e6d19d0685bf617b43b07929f5c5bd0e33bf
x64linux=4c38d2a841e201fcb49c4e0c1d80e6d19d0685bf617b43b07929f5c5bd0e33bf
x64elf=4c38d2a841e201fcb49c4e0c1d80e6d19d0685bf617b43b07929f5c5bd0e33bf
x64v1mac=7c58e2712841fe9eed7cc27a6e428189c4d7f290b000220bacb3edf75635ee32
x64v1win=eab23aaddafaa0228e879af809afd16370ece3fcc579fe5ab184602a62a6ab4c
x64v1mingw=eab23aaddafaa0228e879af809afd16370ece3fcc579fe5ab184602a62a6ab4c
x64v1freebsd=4ed981504e0ee18949d5902f5844c9f3c818e69f461bdd31b7d799f7add62b15
x64v1openbsd=42e3f7a307b47185bed59dc7864d9cb6f76f08d1ff3f13001c130c734f83c346
x64v1netbsd=4c38d2a841e201fcb49c4e0c1d80e6d19d0685bf617b43b07929f5c5bd0e33bf
x64v1musl=4c38d2a841e201fcb49c4e0c1d80e6d19d0685bf617b43b07929f5c5bd0e33bf
x64v1glibc=4c38d2a841e201fcb49c4e0c1d80e6d19d0685bf617b43b07929f5c5bd0e33bf
x64v1linux=4c38d2a841e201fcb49c4e0c1d80e6d19d0685bf617b43b07929f5c5bd0e33bf
x64v1elf=4c38d2a841e201fcb49c4e0c1d80e6d19d0685bf617b43b07929f5c5bd0e33bf
arm64mac=dda0604a3f5bf386d85dfe83386e4217c3571b55f5013171ad325e2e07fc78c5
arm64win=9b09589eca691dee5263fb07a3968f55d3175a81cf7025997d38a31b7aad6b60
arm64mingw=9b09589eca691dee5263fb07a3968f55d3175a81cf7025997d38a31b7aad6b60
arm64linux=d74e0465304f916fe131302be114ab5983ebee418ce931cec3d73619bef988a9
arm64musl=d74e0465304f916fe131302be114ab5983ebee418ce931cec3d73619bef988a9
arm64glibc=d74e0465304f916fe131302be114ab5983ebee418ce931cec3d73619bef988a9
arm64v1win=9b09589eca691dee5263fb07a3968f55d3175a81cf7025997d38a31b7aad6b60
arm64v1mingw=9b09589eca691dee5263fb07a3968f55d3175a81cf7025997d38a31b7aad6b60
arm64v1linux=d74e0465304f916fe131302be114ab5983ebee418ce931cec3d73619bef988a9
arm64v1musl=d74e0465304f916fe131302be114ab5983ebee418ce931cec3d73619bef988a9
arm64v1glibc=d74e0465304f916fe131302be114ab5983ebee418ce931cec3d73619bef988a9
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
