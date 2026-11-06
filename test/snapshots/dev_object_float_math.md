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
x64mac=055b8d12b55d60a76ec416321ee68d203f000a76e02bfb1e52d0f191aded42b7
x64win=221287db165c224cdb839b3e17c7a753f80135a3d30ba32058b04a6ca3becdfe
x64mingw=221287db165c224cdb839b3e17c7a753f80135a3d30ba32058b04a6ca3becdfe
x64freebsd=d5661ffdf5bab45803e89c156f1fd8048c6db49075f291f055ae502728d23d7f
x64openbsd=f9ebcd712910d3bb7d2e0c9a2cf0810df0c5631df2822c9f2f7714de7de45391
x64netbsd=963137bd91a46c0aa4d9b5510b9162d077c3c6936c2cab6e89ac4e892be4bc18
x64musl=963137bd91a46c0aa4d9b5510b9162d077c3c6936c2cab6e89ac4e892be4bc18
x64glibc=963137bd91a46c0aa4d9b5510b9162d077c3c6936c2cab6e89ac4e892be4bc18
x64linux=963137bd91a46c0aa4d9b5510b9162d077c3c6936c2cab6e89ac4e892be4bc18
x64elf=963137bd91a46c0aa4d9b5510b9162d077c3c6936c2cab6e89ac4e892be4bc18
x64v1mac=055b8d12b55d60a76ec416321ee68d203f000a76e02bfb1e52d0f191aded42b7
x64v1win=221287db165c224cdb839b3e17c7a753f80135a3d30ba32058b04a6ca3becdfe
x64v1mingw=221287db165c224cdb839b3e17c7a753f80135a3d30ba32058b04a6ca3becdfe
x64v1freebsd=d5661ffdf5bab45803e89c156f1fd8048c6db49075f291f055ae502728d23d7f
x64v1openbsd=f9ebcd712910d3bb7d2e0c9a2cf0810df0c5631df2822c9f2f7714de7de45391
x64v1netbsd=963137bd91a46c0aa4d9b5510b9162d077c3c6936c2cab6e89ac4e892be4bc18
x64v1musl=963137bd91a46c0aa4d9b5510b9162d077c3c6936c2cab6e89ac4e892be4bc18
x64v1glibc=963137bd91a46c0aa4d9b5510b9162d077c3c6936c2cab6e89ac4e892be4bc18
x64v1linux=963137bd91a46c0aa4d9b5510b9162d077c3c6936c2cab6e89ac4e892be4bc18
x64v1elf=963137bd91a46c0aa4d9b5510b9162d077c3c6936c2cab6e89ac4e892be4bc18
arm64mac=7c67ab9474f3d234d449e3ff16823ef95096176fed503ba98f6f3ecfcaa74acd
arm64win=5a813c94ae3c415ca21a07426181e84143fbd1cac2585827aeae445a825214c2
arm64mingw=5a813c94ae3c415ca21a07426181e84143fbd1cac2585827aeae445a825214c2
arm64linux=51afc11d202bbaacdc26c45bd7ca727f98cab3b57684ef571f8959a96bc5d17a
arm64musl=51afc11d202bbaacdc26c45bd7ca727f98cab3b57684ef571f8959a96bc5d17a
arm64glibc=51afc11d202bbaacdc26c45bd7ca727f98cab3b57684ef571f8959a96bc5d17a
arm64v1win=5a813c94ae3c415ca21a07426181e84143fbd1cac2585827aeae445a825214c2
arm64v1mingw=5a813c94ae3c415ca21a07426181e84143fbd1cac2585827aeae445a825214c2
arm64v1linux=51afc11d202bbaacdc26c45bd7ca727f98cab3b57684ef571f8959a96bc5d17a
arm64v1musl=51afc11d202bbaacdc26c45bd7ca727f98cab3b57684ef571f8959a96bc5d17a
arm64v1glibc=51afc11d202bbaacdc26c45bd7ca727f98cab3b57684ef571f8959a96bc5d17a
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
