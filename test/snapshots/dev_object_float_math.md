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
x64mac=ad5055203afbf5bb2ae749b4a18655b630ed1174b259f9bfa9a0699791147a03
x64win=574f496ba24471543357744b81c8451a4d7dd3015cf76186a7601f55a04641ea
x64mingw=574f496ba24471543357744b81c8451a4d7dd3015cf76186a7601f55a04641ea
x64freebsd=dea1081cfa02b0f1b0526b59b767d72c70610f2a90920d433ec0b845f2625a02
x64openbsd=0cffea314410a2514486328e5d23c6253f2206ca0cbedfb9086b7362499ece15
x64netbsd=9b77255a1f40e11168309b8efc6898d60254d73a448d31909bd5a96486b778b6
x64musl=9b77255a1f40e11168309b8efc6898d60254d73a448d31909bd5a96486b778b6
x64glibc=9b77255a1f40e11168309b8efc6898d60254d73a448d31909bd5a96486b778b6
x64linux=9b77255a1f40e11168309b8efc6898d60254d73a448d31909bd5a96486b778b6
x64elf=9b77255a1f40e11168309b8efc6898d60254d73a448d31909bd5a96486b778b6
x64v1mac=ad5055203afbf5bb2ae749b4a18655b630ed1174b259f9bfa9a0699791147a03
x64v1win=574f496ba24471543357744b81c8451a4d7dd3015cf76186a7601f55a04641ea
x64v1mingw=574f496ba24471543357744b81c8451a4d7dd3015cf76186a7601f55a04641ea
x64v1freebsd=dea1081cfa02b0f1b0526b59b767d72c70610f2a90920d433ec0b845f2625a02
x64v1openbsd=0cffea314410a2514486328e5d23c6253f2206ca0cbedfb9086b7362499ece15
x64v1netbsd=9b77255a1f40e11168309b8efc6898d60254d73a448d31909bd5a96486b778b6
x64v1musl=9b77255a1f40e11168309b8efc6898d60254d73a448d31909bd5a96486b778b6
x64v1glibc=9b77255a1f40e11168309b8efc6898d60254d73a448d31909bd5a96486b778b6
x64v1linux=9b77255a1f40e11168309b8efc6898d60254d73a448d31909bd5a96486b778b6
x64v1elf=9b77255a1f40e11168309b8efc6898d60254d73a448d31909bd5a96486b778b6
arm64mac=f5595b97f6a249e07487df47bd73a8f863b2424f340d9672dd2c3e3248d0247d
arm64win=59914fff9723c1f0ecb3d5b440d1bf28059a463af680c2029aeac9cc76210338
arm64mingw=59914fff9723c1f0ecb3d5b440d1bf28059a463af680c2029aeac9cc76210338
arm64linux=e7a7b0d717c72871870d570c7d367088cc10773ded01e666f38199f494d3b641
arm64musl=e7a7b0d717c72871870d570c7d367088cc10773ded01e666f38199f494d3b641
arm64glibc=e7a7b0d717c72871870d570c7d367088cc10773ded01e666f38199f494d3b641
arm64v1win=59914fff9723c1f0ecb3d5b440d1bf28059a463af680c2029aeac9cc76210338
arm64v1mingw=59914fff9723c1f0ecb3d5b440d1bf28059a463af680c2029aeac9cc76210338
arm64v1linux=e7a7b0d717c72871870d570c7d367088cc10773ded01e666f38199f494d3b641
arm64v1musl=e7a7b0d717c72871870d570c7d367088cc10773ded01e666f38199f494d3b641
arm64v1glibc=e7a7b0d717c72871870d570c7d367088cc10773ded01e666f38199f494d3b641
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
