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
x64mac=04c7c2afbe3d34b3518d60effb80eab9137c94365613bacc3aef378ce78a55af
x64win=e6a868678f6c8b09ae3b7c2fe31024a346c5400c5e0c9b30aedf0f4ae1bd6330
x64mingw=e6a868678f6c8b09ae3b7c2fe31024a346c5400c5e0c9b30aedf0f4ae1bd6330
x64freebsd=4d03f98f00ab6f5792d2f115b7fe53f6ce04b9fdff79472a298ee93c8496e6c3
x64openbsd=cfe3c83a11d60225d2df5983111660550a4b56fc14dee9773f0697aa8e6c8292
x64netbsd=1b625f5b04f81509b52f6e5fdcc937edde44189e382d2d72c3f12726612ae09a
x64musl=1b625f5b04f81509b52f6e5fdcc937edde44189e382d2d72c3f12726612ae09a
x64glibc=1b625f5b04f81509b52f6e5fdcc937edde44189e382d2d72c3f12726612ae09a
x64linux=1b625f5b04f81509b52f6e5fdcc937edde44189e382d2d72c3f12726612ae09a
x64elf=1b625f5b04f81509b52f6e5fdcc937edde44189e382d2d72c3f12726612ae09a
x64v1mac=04c7c2afbe3d34b3518d60effb80eab9137c94365613bacc3aef378ce78a55af
x64v1win=e6a868678f6c8b09ae3b7c2fe31024a346c5400c5e0c9b30aedf0f4ae1bd6330
x64v1mingw=e6a868678f6c8b09ae3b7c2fe31024a346c5400c5e0c9b30aedf0f4ae1bd6330
x64v1freebsd=4d03f98f00ab6f5792d2f115b7fe53f6ce04b9fdff79472a298ee93c8496e6c3
x64v1openbsd=cfe3c83a11d60225d2df5983111660550a4b56fc14dee9773f0697aa8e6c8292
x64v1netbsd=1b625f5b04f81509b52f6e5fdcc937edde44189e382d2d72c3f12726612ae09a
x64v1musl=1b625f5b04f81509b52f6e5fdcc937edde44189e382d2d72c3f12726612ae09a
x64v1glibc=1b625f5b04f81509b52f6e5fdcc937edde44189e382d2d72c3f12726612ae09a
x64v1linux=1b625f5b04f81509b52f6e5fdcc937edde44189e382d2d72c3f12726612ae09a
x64v1elf=1b625f5b04f81509b52f6e5fdcc937edde44189e382d2d72c3f12726612ae09a
arm64mac=4ec0f7f0f9622eb4dfc973f278495165380cbeb7fc1a2ebcfab961e91c2cd3c7
arm64win=0b3316b0759577ec34dd916aacd20ef7195a669f6b9412f1750c211af3c31e71
arm64mingw=0b3316b0759577ec34dd916aacd20ef7195a669f6b9412f1750c211af3c31e71
arm64linux=3602b636030b57b858fda9c41de7c7430295ea87a1e57ecff79453797f4ac5e8
arm64musl=3602b636030b57b858fda9c41de7c7430295ea87a1e57ecff79453797f4ac5e8
arm64glibc=3602b636030b57b858fda9c41de7c7430295ea87a1e57ecff79453797f4ac5e8
arm64v1win=0b3316b0759577ec34dd916aacd20ef7195a669f6b9412f1750c211af3c31e71
arm64v1mingw=0b3316b0759577ec34dd916aacd20ef7195a669f6b9412f1750c211af3c31e71
arm64v1linux=3602b636030b57b858fda9c41de7c7430295ea87a1e57ecff79453797f4ac5e8
arm64v1musl=3602b636030b57b858fda9c41de7c7430295ea87a1e57ecff79453797f4ac5e8
arm64v1glibc=3602b636030b57b858fda9c41de7c7430295ea87a1e57ecff79453797f4ac5e8
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
