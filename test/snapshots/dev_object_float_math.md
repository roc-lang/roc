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
x64mac=7bfe451c749bc73c705eac58a088e6ef94c17f65a7a68f13bad910833afe0b60
x64win=aed596bf69d9ef6a0ae85b07297e0e3f4d96b950a2bf63e6df7a8bce969cfc14
x64mingw=aed596bf69d9ef6a0ae85b07297e0e3f4d96b950a2bf63e6df7a8bce969cfc14
x64freebsd=3b9e08f6413ee9bab53f6a61873746d717c3651463fbd1b5c556e414e7fd56a8
x64openbsd=1435f4c6e004b96c853bec236ac9eafca272ac370c08693dfd3de2f577718466
x64netbsd=a1819b9a6fd3ed1bf77028d5ed6901bd1d2c86006cf09d32b48091692db2402e
x64musl=a1819b9a6fd3ed1bf77028d5ed6901bd1d2c86006cf09d32b48091692db2402e
x64glibc=a1819b9a6fd3ed1bf77028d5ed6901bd1d2c86006cf09d32b48091692db2402e
x64linux=a1819b9a6fd3ed1bf77028d5ed6901bd1d2c86006cf09d32b48091692db2402e
x64elf=a1819b9a6fd3ed1bf77028d5ed6901bd1d2c86006cf09d32b48091692db2402e
x64v1mac=7bfe451c749bc73c705eac58a088e6ef94c17f65a7a68f13bad910833afe0b60
x64v1win=aed596bf69d9ef6a0ae85b07297e0e3f4d96b950a2bf63e6df7a8bce969cfc14
x64v1mingw=aed596bf69d9ef6a0ae85b07297e0e3f4d96b950a2bf63e6df7a8bce969cfc14
x64v1freebsd=3b9e08f6413ee9bab53f6a61873746d717c3651463fbd1b5c556e414e7fd56a8
x64v1openbsd=1435f4c6e004b96c853bec236ac9eafca272ac370c08693dfd3de2f577718466
x64v1netbsd=a1819b9a6fd3ed1bf77028d5ed6901bd1d2c86006cf09d32b48091692db2402e
x64v1musl=a1819b9a6fd3ed1bf77028d5ed6901bd1d2c86006cf09d32b48091692db2402e
x64v1glibc=a1819b9a6fd3ed1bf77028d5ed6901bd1d2c86006cf09d32b48091692db2402e
x64v1linux=a1819b9a6fd3ed1bf77028d5ed6901bd1d2c86006cf09d32b48091692db2402e
x64v1elf=a1819b9a6fd3ed1bf77028d5ed6901bd1d2c86006cf09d32b48091692db2402e
arm64mac=4e068be98bff771f823dcb5c84f7f6ed816b58e86917bbcdce448b36053a2174
arm64win=1a3dcc5b4d8db65e11b256eb2c5be288d6b2ba2d39606baa2855a8f98a41fd85
arm64mingw=1a3dcc5b4d8db65e11b256eb2c5be288d6b2ba2d39606baa2855a8f98a41fd85
arm64linux=2a268e77330be1655ba29e1ba3861e42cbf75b670d36496dd7a7f711d0bec9bf
arm64musl=2a268e77330be1655ba29e1ba3861e42cbf75b670d36496dd7a7f711d0bec9bf
arm64glibc=2a268e77330be1655ba29e1ba3861e42cbf75b670d36496dd7a7f711d0bec9bf
arm64v1win=1a3dcc5b4d8db65e11b256eb2c5be288d6b2ba2d39606baa2855a8f98a41fd85
arm64v1mingw=1a3dcc5b4d8db65e11b256eb2c5be288d6b2ba2d39606baa2855a8f98a41fd85
arm64v1linux=2a268e77330be1655ba29e1ba3861e42cbf75b670d36496dd7a7f711d0bec9bf
arm64v1musl=2a268e77330be1655ba29e1ba3861e42cbf75b670d36496dd7a7f711d0bec9bf
arm64v1glibc=2a268e77330be1655ba29e1ba3861e42cbf75b670d36496dd7a7f711d0bec9bf
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
