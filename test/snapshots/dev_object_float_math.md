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
x64mac=094e52d2cd351738c7792ca05abf077efb93e1ae0b99af8b409c50df0839b0f9
x64win=c81579f22cd20215c64eb03bb87b81d5d59951987e507fba502e3e36aacadc15
x64mingw=c81579f22cd20215c64eb03bb87b81d5d59951987e507fba502e3e36aacadc15
x64freebsd=b80f97430d82ade92a393215df01763439070e3a699b57450c8554172a62e2f4
x64openbsd=c4ee6c2452ae4dbe0e26269ab97172e3beb6123d39d9e2a0a00cb0cc180c39a0
x64netbsd=66f180e2fb85b0e71e8e49a5790eef002dadf2e2ee31b908934dae3393978bf7
x64musl=66f180e2fb85b0e71e8e49a5790eef002dadf2e2ee31b908934dae3393978bf7
x64glibc=66f180e2fb85b0e71e8e49a5790eef002dadf2e2ee31b908934dae3393978bf7
x64linux=66f180e2fb85b0e71e8e49a5790eef002dadf2e2ee31b908934dae3393978bf7
x64elf=66f180e2fb85b0e71e8e49a5790eef002dadf2e2ee31b908934dae3393978bf7
x64v1mac=094e52d2cd351738c7792ca05abf077efb93e1ae0b99af8b409c50df0839b0f9
x64v1win=c81579f22cd20215c64eb03bb87b81d5d59951987e507fba502e3e36aacadc15
x64v1mingw=c81579f22cd20215c64eb03bb87b81d5d59951987e507fba502e3e36aacadc15
x64v1freebsd=b80f97430d82ade92a393215df01763439070e3a699b57450c8554172a62e2f4
x64v1openbsd=c4ee6c2452ae4dbe0e26269ab97172e3beb6123d39d9e2a0a00cb0cc180c39a0
x64v1netbsd=66f180e2fb85b0e71e8e49a5790eef002dadf2e2ee31b908934dae3393978bf7
x64v1musl=66f180e2fb85b0e71e8e49a5790eef002dadf2e2ee31b908934dae3393978bf7
x64v1glibc=66f180e2fb85b0e71e8e49a5790eef002dadf2e2ee31b908934dae3393978bf7
x64v1linux=66f180e2fb85b0e71e8e49a5790eef002dadf2e2ee31b908934dae3393978bf7
x64v1elf=66f180e2fb85b0e71e8e49a5790eef002dadf2e2ee31b908934dae3393978bf7
arm64mac=8dba50fb432d0d205129362bdfaf67c7137fd69e0c40b96ad86fdbbbc239d663
arm64win=8c702fb0b8a62687a88e29fe7478e0caac170b6c6df23c2f01876dc6ca947ff7
arm64mingw=8c702fb0b8a62687a88e29fe7478e0caac170b6c6df23c2f01876dc6ca947ff7
arm64linux=b712d4d61346cdd6921f129976a8d2b10818e3e1a989103ecb510a1b1f8b85ad
arm64musl=b712d4d61346cdd6921f129976a8d2b10818e3e1a989103ecb510a1b1f8b85ad
arm64glibc=b712d4d61346cdd6921f129976a8d2b10818e3e1a989103ecb510a1b1f8b85ad
arm64v1win=8c702fb0b8a62687a88e29fe7478e0caac170b6c6df23c2f01876dc6ca947ff7
arm64v1mingw=8c702fb0b8a62687a88e29fe7478e0caac170b6c6df23c2f01876dc6ca947ff7
arm64v1linux=b712d4d61346cdd6921f129976a8d2b10818e3e1a989103ecb510a1b1f8b85ad
arm64v1musl=b712d4d61346cdd6921f129976a8d2b10818e3e1a989103ecb510a1b1f8b85ad
arm64v1glibc=b712d4d61346cdd6921f129976a8d2b10818e3e1a989103ecb510a1b1f8b85ad
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
