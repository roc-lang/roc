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
x64mac=7aaef86e49cb022e2629f1d97adf70d312bf432211e386eb28d263ae0ad413d4
x64win=3d8439fe5c6ac2ea1a50481dfc43bc46c2d61979abda6755133261043554a62c
x64mingw=3d8439fe5c6ac2ea1a50481dfc43bc46c2d61979abda6755133261043554a62c
x64freebsd=06f5b65664a026cb0fdaf63b83ce87acddfc62ee5089de7c84006fc9ceb8febf
x64openbsd=3d4a9af6a23c33e48d1761df104404252db636495c5f64aa4117164cc23fe043
x64netbsd=66e234a6f779b0ae3c0b35c44777285f2ce028785ec90140039f766bd52ad8ef
x64musl=66e234a6f779b0ae3c0b35c44777285f2ce028785ec90140039f766bd52ad8ef
x64glibc=66e234a6f779b0ae3c0b35c44777285f2ce028785ec90140039f766bd52ad8ef
x64linux=66e234a6f779b0ae3c0b35c44777285f2ce028785ec90140039f766bd52ad8ef
x64elf=66e234a6f779b0ae3c0b35c44777285f2ce028785ec90140039f766bd52ad8ef
x64v1mac=7aaef86e49cb022e2629f1d97adf70d312bf432211e386eb28d263ae0ad413d4
x64v1win=3d8439fe5c6ac2ea1a50481dfc43bc46c2d61979abda6755133261043554a62c
x64v1mingw=3d8439fe5c6ac2ea1a50481dfc43bc46c2d61979abda6755133261043554a62c
x64v1freebsd=06f5b65664a026cb0fdaf63b83ce87acddfc62ee5089de7c84006fc9ceb8febf
x64v1openbsd=3d4a9af6a23c33e48d1761df104404252db636495c5f64aa4117164cc23fe043
x64v1netbsd=66e234a6f779b0ae3c0b35c44777285f2ce028785ec90140039f766bd52ad8ef
x64v1musl=66e234a6f779b0ae3c0b35c44777285f2ce028785ec90140039f766bd52ad8ef
x64v1glibc=66e234a6f779b0ae3c0b35c44777285f2ce028785ec90140039f766bd52ad8ef
x64v1linux=66e234a6f779b0ae3c0b35c44777285f2ce028785ec90140039f766bd52ad8ef
x64v1elf=66e234a6f779b0ae3c0b35c44777285f2ce028785ec90140039f766bd52ad8ef
arm64mac=d178993b2c565950848e17c22d30744a4a3f379759272aafecd6ab68b50842f5
arm64win=dec4bafd2523a299284a07914cb3bb74332e23baac9fd282ddd85fd1f1c83e91
arm64mingw=dec4bafd2523a299284a07914cb3bb74332e23baac9fd282ddd85fd1f1c83e91
arm64linux=663905baf07b223266b4ab821f40c45dfdc083825fe353a889b9d9a04ae35136
arm64musl=663905baf07b223266b4ab821f40c45dfdc083825fe353a889b9d9a04ae35136
arm64glibc=663905baf07b223266b4ab821f40c45dfdc083825fe353a889b9d9a04ae35136
arm64v1win=dec4bafd2523a299284a07914cb3bb74332e23baac9fd282ddd85fd1f1c83e91
arm64v1mingw=dec4bafd2523a299284a07914cb3bb74332e23baac9fd282ddd85fd1f1c83e91
arm64v1linux=663905baf07b223266b4ab821f40c45dfdc083825fe353a889b9d9a04ae35136
arm64v1musl=663905baf07b223266b4ab821f40c45dfdc083825fe353a889b9d9a04ae35136
arm64v1glibc=663905baf07b223266b4ab821f40c45dfdc083825fe353a889b9d9a04ae35136
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
