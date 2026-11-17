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
x64mac=c92527613f8fd53b8c3482ea7c239085a07d4e11179293f470cd166f59fb8e9e
x64win=604e473bd1fa8f972efd9239aad633134fe6eaf9d4fbb64bd3047e7db7901bbf
x64mingw=604e473bd1fa8f972efd9239aad633134fe6eaf9d4fbb64bd3047e7db7901bbf
x64freebsd=9baa2c8d0dfd3141e528ea79f5ee03edbb4340cc3d263f5e9db5004adb98c4ba
x64openbsd=068ec7e58201e793bc99eaa2ead88bafa378b68e7baf425d54a9ac543e11245c
x64netbsd=4b0bf05bba1d99e7645c89a691034213ecbd24e6c25abe9f03e90ed5e35151af
x64musl=4b0bf05bba1d99e7645c89a691034213ecbd24e6c25abe9f03e90ed5e35151af
x64glibc=4b0bf05bba1d99e7645c89a691034213ecbd24e6c25abe9f03e90ed5e35151af
x64linux=4b0bf05bba1d99e7645c89a691034213ecbd24e6c25abe9f03e90ed5e35151af
x64elf=4b0bf05bba1d99e7645c89a691034213ecbd24e6c25abe9f03e90ed5e35151af
x64v1mac=c92527613f8fd53b8c3482ea7c239085a07d4e11179293f470cd166f59fb8e9e
x64v1win=604e473bd1fa8f972efd9239aad633134fe6eaf9d4fbb64bd3047e7db7901bbf
x64v1mingw=604e473bd1fa8f972efd9239aad633134fe6eaf9d4fbb64bd3047e7db7901bbf
x64v1freebsd=9baa2c8d0dfd3141e528ea79f5ee03edbb4340cc3d263f5e9db5004adb98c4ba
x64v1openbsd=068ec7e58201e793bc99eaa2ead88bafa378b68e7baf425d54a9ac543e11245c
x64v1netbsd=4b0bf05bba1d99e7645c89a691034213ecbd24e6c25abe9f03e90ed5e35151af
x64v1musl=4b0bf05bba1d99e7645c89a691034213ecbd24e6c25abe9f03e90ed5e35151af
x64v1glibc=4b0bf05bba1d99e7645c89a691034213ecbd24e6c25abe9f03e90ed5e35151af
x64v1linux=4b0bf05bba1d99e7645c89a691034213ecbd24e6c25abe9f03e90ed5e35151af
x64v1elf=4b0bf05bba1d99e7645c89a691034213ecbd24e6c25abe9f03e90ed5e35151af
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
