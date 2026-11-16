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
x64mac=e2c55e3b0475ab50decfb795840392c217834c5a0e5f3b1876251e9504d2f7c1
x64win=864e531b129738385303814023c5f2d1f2fea7f040224f5a6ea76336341c1e1c
x64mingw=864e531b129738385303814023c5f2d1f2fea7f040224f5a6ea76336341c1e1c
x64freebsd=b3cd75fd79b1fac31571cd178923349fb7483ceb1ece3fd16d4bf7ab817b43ba
x64openbsd=7d5f3962d7253a15979e2fb9da201ab7451703c446fc13d69a80c827951e3a8f
x64netbsd=485a204cbb599784f512d9139f33c836a328a6154b8f6fd23659171e2ab7cac2
x64musl=485a204cbb599784f512d9139f33c836a328a6154b8f6fd23659171e2ab7cac2
x64glibc=485a204cbb599784f512d9139f33c836a328a6154b8f6fd23659171e2ab7cac2
x64linux=485a204cbb599784f512d9139f33c836a328a6154b8f6fd23659171e2ab7cac2
x64elf=485a204cbb599784f512d9139f33c836a328a6154b8f6fd23659171e2ab7cac2
x64v1mac=e2c55e3b0475ab50decfb795840392c217834c5a0e5f3b1876251e9504d2f7c1
x64v1win=864e531b129738385303814023c5f2d1f2fea7f040224f5a6ea76336341c1e1c
x64v1mingw=864e531b129738385303814023c5f2d1f2fea7f040224f5a6ea76336341c1e1c
x64v1freebsd=b3cd75fd79b1fac31571cd178923349fb7483ceb1ece3fd16d4bf7ab817b43ba
x64v1openbsd=7d5f3962d7253a15979e2fb9da201ab7451703c446fc13d69a80c827951e3a8f
x64v1netbsd=485a204cbb599784f512d9139f33c836a328a6154b8f6fd23659171e2ab7cac2
x64v1musl=485a204cbb599784f512d9139f33c836a328a6154b8f6fd23659171e2ab7cac2
x64v1glibc=485a204cbb599784f512d9139f33c836a328a6154b8f6fd23659171e2ab7cac2
x64v1linux=485a204cbb599784f512d9139f33c836a328a6154b8f6fd23659171e2ab7cac2
x64v1elf=485a204cbb599784f512d9139f33c836a328a6154b8f6fd23659171e2ab7cac2
arm64mac=3ca2b02c80d3737ef54eaf15b7d9e3993ced3ce4eb3c10826c86ec00e8b235ef
arm64win=347a2751ce7cc38856da38bc466e5ca1cbacfba2de060791a06dfe160ee53c4a
arm64mingw=347a2751ce7cc38856da38bc466e5ca1cbacfba2de060791a06dfe160ee53c4a
arm64linux=c054d1cb50a3f503def3a3f40609425445b75b4a5cbd2da8d6473b2132882c81
arm64musl=c054d1cb50a3f503def3a3f40609425445b75b4a5cbd2da8d6473b2132882c81
arm64glibc=c054d1cb50a3f503def3a3f40609425445b75b4a5cbd2da8d6473b2132882c81
arm64v1win=347a2751ce7cc38856da38bc466e5ca1cbacfba2de060791a06dfe160ee53c4a
arm64v1mingw=347a2751ce7cc38856da38bc466e5ca1cbacfba2de060791a06dfe160ee53c4a
arm64v1linux=c054d1cb50a3f503def3a3f40609425445b75b4a5cbd2da8d6473b2132882c81
arm64v1musl=c054d1cb50a3f503def3a3f40609425445b75b4a5cbd2da8d6473b2132882c81
arm64v1glibc=c054d1cb50a3f503def3a3f40609425445b75b4a5cbd2da8d6473b2132882c81
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
