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
x64mac=4780fce9da01af9147f8d555274301002451915896401f81a9fb18aafaaff943
x64win=069912657e6f48abbb136f0064cd8bc22bcedb2e6d14c12964205198936e0561
x64mingw=069912657e6f48abbb136f0064cd8bc22bcedb2e6d14c12964205198936e0561
x64freebsd=c1669520be55544e97745a6944e00d3784c26e414b0b5690a0ce121a73dcec9b
x64openbsd=e84fbe18c05008732863efb47328d1b706410a0560d94eb539a01199c95adc36
x64netbsd=660c9748cbd16f3a175ca76f65312a76e88fa89e7f91742375f40887e29460c7
x64musl=660c9748cbd16f3a175ca76f65312a76e88fa89e7f91742375f40887e29460c7
x64glibc=660c9748cbd16f3a175ca76f65312a76e88fa89e7f91742375f40887e29460c7
x64linux=660c9748cbd16f3a175ca76f65312a76e88fa89e7f91742375f40887e29460c7
x64elf=660c9748cbd16f3a175ca76f65312a76e88fa89e7f91742375f40887e29460c7
x64v1mac=4780fce9da01af9147f8d555274301002451915896401f81a9fb18aafaaff943
x64v1win=069912657e6f48abbb136f0064cd8bc22bcedb2e6d14c12964205198936e0561
x64v1mingw=069912657e6f48abbb136f0064cd8bc22bcedb2e6d14c12964205198936e0561
x64v1freebsd=c1669520be55544e97745a6944e00d3784c26e414b0b5690a0ce121a73dcec9b
x64v1openbsd=e84fbe18c05008732863efb47328d1b706410a0560d94eb539a01199c95adc36
x64v1netbsd=660c9748cbd16f3a175ca76f65312a76e88fa89e7f91742375f40887e29460c7
x64v1musl=660c9748cbd16f3a175ca76f65312a76e88fa89e7f91742375f40887e29460c7
x64v1glibc=660c9748cbd16f3a175ca76f65312a76e88fa89e7f91742375f40887e29460c7
x64v1linux=660c9748cbd16f3a175ca76f65312a76e88fa89e7f91742375f40887e29460c7
x64v1elf=660c9748cbd16f3a175ca76f65312a76e88fa89e7f91742375f40887e29460c7
arm64mac=e6ce75c7b065220a6fd4dc7cb4be4b22b1b5f34831db247c023efed093c5a657
arm64win=e363f98f3ca8a5a6e8bd9c373c451c49da829921d663364e4fed1714903493a6
arm64mingw=e363f98f3ca8a5a6e8bd9c373c451c49da829921d663364e4fed1714903493a6
arm64linux=035f4a3a431fc67d0904bb227ac4d0494cb461c67b7111737c34253acd14c4ec
arm64musl=035f4a3a431fc67d0904bb227ac4d0494cb461c67b7111737c34253acd14c4ec
arm64glibc=035f4a3a431fc67d0904bb227ac4d0494cb461c67b7111737c34253acd14c4ec
arm64v1win=e363f98f3ca8a5a6e8bd9c373c451c49da829921d663364e4fed1714903493a6
arm64v1mingw=e363f98f3ca8a5a6e8bd9c373c451c49da829921d663364e4fed1714903493a6
arm64v1linux=035f4a3a431fc67d0904bb227ac4d0494cb461c67b7111737c34253acd14c4ec
arm64v1musl=035f4a3a431fc67d0904bb227ac4d0494cb461c67b7111737c34253acd14c4ec
arm64v1glibc=035f4a3a431fc67d0904bb227ac4d0494cb461c67b7111737c34253acd14c4ec
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
