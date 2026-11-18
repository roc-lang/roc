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
x64mac=4c004e7ef51d8c1d2adf915f4a3ec425a032545a7e81676c022f0c41f70f08dd
x64win=4a553e02fa6853482d23d7aede83de0b6006f82fe2020be6488902d57a80bb84
x64mingw=4a553e02fa6853482d23d7aede83de0b6006f82fe2020be6488902d57a80bb84
x64freebsd=ddf0f485f99c5f913a1302fa786f662ea4d8c4f6dac91c753595b1ab923a45c2
x64openbsd=9ee96682980ca964103dd181df7a8ec11c7994210c535574947d04dae5f44041
x64netbsd=90786643b82181aae58dc88fe9558e849ee146631d36a034474518c871c5fbfc
x64musl=90786643b82181aae58dc88fe9558e849ee146631d36a034474518c871c5fbfc
x64glibc=90786643b82181aae58dc88fe9558e849ee146631d36a034474518c871c5fbfc
x64linux=90786643b82181aae58dc88fe9558e849ee146631d36a034474518c871c5fbfc
x64elf=90786643b82181aae58dc88fe9558e849ee146631d36a034474518c871c5fbfc
x64v1mac=4c004e7ef51d8c1d2adf915f4a3ec425a032545a7e81676c022f0c41f70f08dd
x64v1win=4a553e02fa6853482d23d7aede83de0b6006f82fe2020be6488902d57a80bb84
x64v1mingw=4a553e02fa6853482d23d7aede83de0b6006f82fe2020be6488902d57a80bb84
x64v1freebsd=ddf0f485f99c5f913a1302fa786f662ea4d8c4f6dac91c753595b1ab923a45c2
x64v1openbsd=9ee96682980ca964103dd181df7a8ec11c7994210c535574947d04dae5f44041
x64v1netbsd=90786643b82181aae58dc88fe9558e849ee146631d36a034474518c871c5fbfc
x64v1musl=90786643b82181aae58dc88fe9558e849ee146631d36a034474518c871c5fbfc
x64v1glibc=90786643b82181aae58dc88fe9558e849ee146631d36a034474518c871c5fbfc
x64v1linux=90786643b82181aae58dc88fe9558e849ee146631d36a034474518c871c5fbfc
x64v1elf=90786643b82181aae58dc88fe9558e849ee146631d36a034474518c871c5fbfc
arm64mac=74223b6bfbc524bc8ff8b7e3d0a15ac1951ac8e929669e9ba8ccf945b1ef5936
arm64win=8cfa70fdde507a488edc8faf9ea3eefcf2bf868a2d1b3d85f11106db9180fe3b
arm64mingw=8cfa70fdde507a488edc8faf9ea3eefcf2bf868a2d1b3d85f11106db9180fe3b
arm64linux=3fe900a8a1de3e3bbefa3c4357df833248ad5fdd56ade5fba0b973e7b7483c28
arm64musl=3fe900a8a1de3e3bbefa3c4357df833248ad5fdd56ade5fba0b973e7b7483c28
arm64glibc=3fe900a8a1de3e3bbefa3c4357df833248ad5fdd56ade5fba0b973e7b7483c28
arm64v1win=8cfa70fdde507a488edc8faf9ea3eefcf2bf868a2d1b3d85f11106db9180fe3b
arm64v1mingw=8cfa70fdde507a488edc8faf9ea3eefcf2bf868a2d1b3d85f11106db9180fe3b
arm64v1linux=3fe900a8a1de3e3bbefa3c4357df833248ad5fdd56ade5fba0b973e7b7483c28
arm64v1musl=3fe900a8a1de3e3bbefa3c4357df833248ad5fdd56ade5fba0b973e7b7483c28
arm64v1glibc=3fe900a8a1de3e3bbefa3c4357df833248ad5fdd56ade5fba0b973e7b7483c28
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
