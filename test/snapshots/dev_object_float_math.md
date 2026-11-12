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
x64mac=511468a67171cfc007cc69cc67028c4e81b2078368f900f68a4430da12fb32ef
x64win=2bd1a05e4ec71d1fdb42c6f658fd5996345d8da913037bc0bbb30757670e0483
x64mingw=2bd1a05e4ec71d1fdb42c6f658fd5996345d8da913037bc0bbb30757670e0483
x64freebsd=8f7e617e4987b135bdb09657b63fd387ace83eaf552482c3dcaac5b9a394eb52
x64openbsd=2994be229236e37a583a33b00820b02a619a8221827e06a243edec31e5ac8e77
x64netbsd=793b385932a60cc3c447f629f25c544212eb0cd0d596fccedc041cbe391f8598
x64musl=793b385932a60cc3c447f629f25c544212eb0cd0d596fccedc041cbe391f8598
x64glibc=793b385932a60cc3c447f629f25c544212eb0cd0d596fccedc041cbe391f8598
x64linux=793b385932a60cc3c447f629f25c544212eb0cd0d596fccedc041cbe391f8598
x64elf=793b385932a60cc3c447f629f25c544212eb0cd0d596fccedc041cbe391f8598
x64v1mac=511468a67171cfc007cc69cc67028c4e81b2078368f900f68a4430da12fb32ef
x64v1win=2bd1a05e4ec71d1fdb42c6f658fd5996345d8da913037bc0bbb30757670e0483
x64v1mingw=2bd1a05e4ec71d1fdb42c6f658fd5996345d8da913037bc0bbb30757670e0483
x64v1freebsd=8f7e617e4987b135bdb09657b63fd387ace83eaf552482c3dcaac5b9a394eb52
x64v1openbsd=2994be229236e37a583a33b00820b02a619a8221827e06a243edec31e5ac8e77
x64v1netbsd=793b385932a60cc3c447f629f25c544212eb0cd0d596fccedc041cbe391f8598
x64v1musl=793b385932a60cc3c447f629f25c544212eb0cd0d596fccedc041cbe391f8598
x64v1glibc=793b385932a60cc3c447f629f25c544212eb0cd0d596fccedc041cbe391f8598
x64v1linux=793b385932a60cc3c447f629f25c544212eb0cd0d596fccedc041cbe391f8598
x64v1elf=793b385932a60cc3c447f629f25c544212eb0cd0d596fccedc041cbe391f8598
arm64mac=1bfe2927a501d4a9506532fcaae27a76e6bfd4c28db52258441e3f595d0791f1
arm64win=9958732fb3ab78e5a95fa43ea37a2e8013fbdd23e90d8dd49a535169929523bd
arm64mingw=9958732fb3ab78e5a95fa43ea37a2e8013fbdd23e90d8dd49a535169929523bd
arm64linux=c005dc1de07c0a627fd4a195045cc318c4b5d25d352f0ec9a8e5d809108123ad
arm64musl=c005dc1de07c0a627fd4a195045cc318c4b5d25d352f0ec9a8e5d809108123ad
arm64glibc=c005dc1de07c0a627fd4a195045cc318c4b5d25d352f0ec9a8e5d809108123ad
arm64v1win=9958732fb3ab78e5a95fa43ea37a2e8013fbdd23e90d8dd49a535169929523bd
arm64v1mingw=9958732fb3ab78e5a95fa43ea37a2e8013fbdd23e90d8dd49a535169929523bd
arm64v1linux=c005dc1de07c0a627fd4a195045cc318c4b5d25d352f0ec9a8e5d809108123ad
arm64v1musl=c005dc1de07c0a627fd4a195045cc318c4b5d25d352f0ec9a8e5d809108123ad
arm64v1glibc=c005dc1de07c0a627fd4a195045cc318c4b5d25d352f0ec9a8e5d809108123ad
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
