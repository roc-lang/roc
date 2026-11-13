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
x64mac=7fa405e3a69e69f32bd72cbc6d8235189fdfe54314866b12afdcc57fcf02b76d
x64win=1df377e4e8bfdcddc3ac70e3939af64ce688e349ca3babde955566a0cb9e02ff
x64mingw=1df377e4e8bfdcddc3ac70e3939af64ce688e349ca3babde955566a0cb9e02ff
x64freebsd=d43bd09eeaf03e6487da4ed5b99c04117e7a6638bed7cc29d6739593c7cd1d82
x64openbsd=56892025a22bbea413d4d76a720e1a7671e606e18209b4fe5a83ab20dc2320fe
x64netbsd=9d418628639df6a13556c6f620f622802f0855b462dbb52f2d50db13a9d2f303
x64musl=9d418628639df6a13556c6f620f622802f0855b462dbb52f2d50db13a9d2f303
x64glibc=9d418628639df6a13556c6f620f622802f0855b462dbb52f2d50db13a9d2f303
x64linux=9d418628639df6a13556c6f620f622802f0855b462dbb52f2d50db13a9d2f303
x64elf=9d418628639df6a13556c6f620f622802f0855b462dbb52f2d50db13a9d2f303
x64v1mac=7fa405e3a69e69f32bd72cbc6d8235189fdfe54314866b12afdcc57fcf02b76d
x64v1win=1df377e4e8bfdcddc3ac70e3939af64ce688e349ca3babde955566a0cb9e02ff
x64v1mingw=1df377e4e8bfdcddc3ac70e3939af64ce688e349ca3babde955566a0cb9e02ff
x64v1freebsd=d43bd09eeaf03e6487da4ed5b99c04117e7a6638bed7cc29d6739593c7cd1d82
x64v1openbsd=56892025a22bbea413d4d76a720e1a7671e606e18209b4fe5a83ab20dc2320fe
x64v1netbsd=9d418628639df6a13556c6f620f622802f0855b462dbb52f2d50db13a9d2f303
x64v1musl=9d418628639df6a13556c6f620f622802f0855b462dbb52f2d50db13a9d2f303
x64v1glibc=9d418628639df6a13556c6f620f622802f0855b462dbb52f2d50db13a9d2f303
x64v1linux=9d418628639df6a13556c6f620f622802f0855b462dbb52f2d50db13a9d2f303
x64v1elf=9d418628639df6a13556c6f620f622802f0855b462dbb52f2d50db13a9d2f303
arm64mac=973724a10ac237aa79963ecb267295444a8fe267cc2df12cdfcab32770125be3
arm64win=580c7c541c9bb0046d3ba44807853f5c2b81f7341dac02206b427190f8abce09
arm64mingw=580c7c541c9bb0046d3ba44807853f5c2b81f7341dac02206b427190f8abce09
arm64linux=cad633690549c294c204455657758145f7dc6532a050349c1fcb6d1fc6fd5aaa
arm64musl=cad633690549c294c204455657758145f7dc6532a050349c1fcb6d1fc6fd5aaa
arm64glibc=cad633690549c294c204455657758145f7dc6532a050349c1fcb6d1fc6fd5aaa
arm64v1win=580c7c541c9bb0046d3ba44807853f5c2b81f7341dac02206b427190f8abce09
arm64v1mingw=580c7c541c9bb0046d3ba44807853f5c2b81f7341dac02206b427190f8abce09
arm64v1linux=cad633690549c294c204455657758145f7dc6532a050349c1fcb6d1fc6fd5aaa
arm64v1musl=cad633690549c294c204455657758145f7dc6532a050349c1fcb6d1fc6fd5aaa
arm64v1glibc=cad633690549c294c204455657758145f7dc6532a050349c1fcb6d1fc6fd5aaa
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
