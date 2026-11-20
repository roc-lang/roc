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
x64mac=31ec89be60f821fbf9dbb77895a6aba651e2b092d552e3d00155d21d010b4d5d
x64win=6287b600cc01f6ade3c097bf2455e209788ab765972875269c718bdc5809bfa4
x64mingw=6287b600cc01f6ade3c097bf2455e209788ab765972875269c718bdc5809bfa4
x64freebsd=684af4ca75013c2c8af8ea82aa03aea46f63580ebc912c7205f0d0a949f3c889
x64openbsd=ae0e6050e9ec6ac558acaa7050b28211c3c2bfd87c87bd74fa2f035451f23ec0
x64netbsd=77b54f842e3fa4eaaaac064a64f2aaec249d028f6e82a4b8ca26b76b97e7cc6b
x64musl=77b54f842e3fa4eaaaac064a64f2aaec249d028f6e82a4b8ca26b76b97e7cc6b
x64glibc=77b54f842e3fa4eaaaac064a64f2aaec249d028f6e82a4b8ca26b76b97e7cc6b
x64linux=77b54f842e3fa4eaaaac064a64f2aaec249d028f6e82a4b8ca26b76b97e7cc6b
x64elf=77b54f842e3fa4eaaaac064a64f2aaec249d028f6e82a4b8ca26b76b97e7cc6b
x64v1mac=31ec89be60f821fbf9dbb77895a6aba651e2b092d552e3d00155d21d010b4d5d
x64v1win=6287b600cc01f6ade3c097bf2455e209788ab765972875269c718bdc5809bfa4
x64v1mingw=6287b600cc01f6ade3c097bf2455e209788ab765972875269c718bdc5809bfa4
x64v1freebsd=684af4ca75013c2c8af8ea82aa03aea46f63580ebc912c7205f0d0a949f3c889
x64v1openbsd=ae0e6050e9ec6ac558acaa7050b28211c3c2bfd87c87bd74fa2f035451f23ec0
x64v1netbsd=77b54f842e3fa4eaaaac064a64f2aaec249d028f6e82a4b8ca26b76b97e7cc6b
x64v1musl=77b54f842e3fa4eaaaac064a64f2aaec249d028f6e82a4b8ca26b76b97e7cc6b
x64v1glibc=77b54f842e3fa4eaaaac064a64f2aaec249d028f6e82a4b8ca26b76b97e7cc6b
x64v1linux=77b54f842e3fa4eaaaac064a64f2aaec249d028f6e82a4b8ca26b76b97e7cc6b
x64v1elf=77b54f842e3fa4eaaaac064a64f2aaec249d028f6e82a4b8ca26b76b97e7cc6b
arm64mac=a710b254e181790bae5178dd2cbf8c40de7c22e308baaf46e7d3cb98782cfc7b
arm64win=043a4c418ab2cdf334966ff0d70017834f868c5b9303ee7da776a7fc8786d006
arm64mingw=043a4c418ab2cdf334966ff0d70017834f868c5b9303ee7da776a7fc8786d006
arm64linux=09caab36f3e4324de2101bf8307b42439f7812cc9e8068d702810f62b07accc9
arm64musl=09caab36f3e4324de2101bf8307b42439f7812cc9e8068d702810f62b07accc9
arm64glibc=09caab36f3e4324de2101bf8307b42439f7812cc9e8068d702810f62b07accc9
arm64v1win=043a4c418ab2cdf334966ff0d70017834f868c5b9303ee7da776a7fc8786d006
arm64v1mingw=043a4c418ab2cdf334966ff0d70017834f868c5b9303ee7da776a7fc8786d006
arm64v1linux=09caab36f3e4324de2101bf8307b42439f7812cc9e8068d702810f62b07accc9
arm64v1musl=09caab36f3e4324de2101bf8307b42439f7812cc9e8068d702810f62b07accc9
arm64v1glibc=09caab36f3e4324de2101bf8307b42439f7812cc9e8068d702810f62b07accc9
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
