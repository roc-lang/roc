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
x64mac=d3bf14c5d4f59009d29adbe2298ca0ab005dcd019125662b0ed4531fd8f128df
x64win=e24a4a8af623e9476a082ee35fb47d167eab86f2cf2b9210618ff52fc07c5410
x64mingw=e24a4a8af623e9476a082ee35fb47d167eab86f2cf2b9210618ff52fc07c5410
x64freebsd=d0841f2b921af54b601e65dc806df97872873f6d4c8c2d1049a1d5a301eb579c
x64openbsd=d7cb273dfc4fa9112191ffa12c23d14ad184304030e3a693ed3f3bf2b5a77086
x64netbsd=5913feb44ad86497a668eaa310d6e0f19505dc9f414b933e1a020b468b226851
x64musl=5913feb44ad86497a668eaa310d6e0f19505dc9f414b933e1a020b468b226851
x64glibc=5913feb44ad86497a668eaa310d6e0f19505dc9f414b933e1a020b468b226851
x64linux=5913feb44ad86497a668eaa310d6e0f19505dc9f414b933e1a020b468b226851
x64elf=5913feb44ad86497a668eaa310d6e0f19505dc9f414b933e1a020b468b226851
x64v1mac=d3bf14c5d4f59009d29adbe2298ca0ab005dcd019125662b0ed4531fd8f128df
x64v1win=e24a4a8af623e9476a082ee35fb47d167eab86f2cf2b9210618ff52fc07c5410
x64v1mingw=e24a4a8af623e9476a082ee35fb47d167eab86f2cf2b9210618ff52fc07c5410
x64v1freebsd=d0841f2b921af54b601e65dc806df97872873f6d4c8c2d1049a1d5a301eb579c
x64v1openbsd=d7cb273dfc4fa9112191ffa12c23d14ad184304030e3a693ed3f3bf2b5a77086
x64v1netbsd=5913feb44ad86497a668eaa310d6e0f19505dc9f414b933e1a020b468b226851
x64v1musl=5913feb44ad86497a668eaa310d6e0f19505dc9f414b933e1a020b468b226851
x64v1glibc=5913feb44ad86497a668eaa310d6e0f19505dc9f414b933e1a020b468b226851
x64v1linux=5913feb44ad86497a668eaa310d6e0f19505dc9f414b933e1a020b468b226851
x64v1elf=5913feb44ad86497a668eaa310d6e0f19505dc9f414b933e1a020b468b226851
arm64mac=bf8ec6172c56400ce273d8939de39fb87f7897f18417e40a93368c1e5509229b
arm64win=6b2e64565fcd2a25a670da21ef2125d5ef5ae7e645795c610024bba3458ab10a
arm64mingw=6b2e64565fcd2a25a670da21ef2125d5ef5ae7e645795c610024bba3458ab10a
arm64linux=d4e2454cc6102a1529344be2ec0978b7c87841681513120a94e0107f02bb7701
arm64musl=d4e2454cc6102a1529344be2ec0978b7c87841681513120a94e0107f02bb7701
arm64glibc=d4e2454cc6102a1529344be2ec0978b7c87841681513120a94e0107f02bb7701
arm64v1win=6b2e64565fcd2a25a670da21ef2125d5ef5ae7e645795c610024bba3458ab10a
arm64v1mingw=6b2e64565fcd2a25a670da21ef2125d5ef5ae7e645795c610024bba3458ab10a
arm64v1linux=d4e2454cc6102a1529344be2ec0978b7c87841681513120a94e0107f02bb7701
arm64v1musl=d4e2454cc6102a1529344be2ec0978b7c87841681513120a94e0107f02bb7701
arm64v1glibc=d4e2454cc6102a1529344be2ec0978b7c87841681513120a94e0107f02bb7701
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
