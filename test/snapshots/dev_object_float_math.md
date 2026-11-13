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
x64mac=5f50e03d5a7a91827ab9c7f596adda541f5126b391f4dc7f82b0058b03753273
x64win=839b7eae97ec9c465f72e328f0fa757961ecea47c102f594782f4802197f6c04
x64mingw=839b7eae97ec9c465f72e328f0fa757961ecea47c102f594782f4802197f6c04
x64freebsd=48ef6eaf3292f5b434d9f4556f246e1e7ab8157dc0c52233826635b362f4dd6f
x64openbsd=26039e160a4f9efbe9cd1a2f4c2483395fbbdf5fd5bbf8b0a3ffc7638dcab440
x64netbsd=e8e03c68ef3a034d21a7c858a3284726d46b98ead431589d70061f1ac9da9ecd
x64musl=e8e03c68ef3a034d21a7c858a3284726d46b98ead431589d70061f1ac9da9ecd
x64glibc=e8e03c68ef3a034d21a7c858a3284726d46b98ead431589d70061f1ac9da9ecd
x64linux=e8e03c68ef3a034d21a7c858a3284726d46b98ead431589d70061f1ac9da9ecd
x64elf=e8e03c68ef3a034d21a7c858a3284726d46b98ead431589d70061f1ac9da9ecd
x64v1mac=5f50e03d5a7a91827ab9c7f596adda541f5126b391f4dc7f82b0058b03753273
x64v1win=839b7eae97ec9c465f72e328f0fa757961ecea47c102f594782f4802197f6c04
x64v1mingw=839b7eae97ec9c465f72e328f0fa757961ecea47c102f594782f4802197f6c04
x64v1freebsd=48ef6eaf3292f5b434d9f4556f246e1e7ab8157dc0c52233826635b362f4dd6f
x64v1openbsd=26039e160a4f9efbe9cd1a2f4c2483395fbbdf5fd5bbf8b0a3ffc7638dcab440
x64v1netbsd=e8e03c68ef3a034d21a7c858a3284726d46b98ead431589d70061f1ac9da9ecd
x64v1musl=e8e03c68ef3a034d21a7c858a3284726d46b98ead431589d70061f1ac9da9ecd
x64v1glibc=e8e03c68ef3a034d21a7c858a3284726d46b98ead431589d70061f1ac9da9ecd
x64v1linux=e8e03c68ef3a034d21a7c858a3284726d46b98ead431589d70061f1ac9da9ecd
x64v1elf=e8e03c68ef3a034d21a7c858a3284726d46b98ead431589d70061f1ac9da9ecd
arm64mac=322b9b5092517baffce43daf14ad227d65c9f0c58a837468287e5f0ad0786f8d
arm64win=2c8012a0b46f9d9e1ab4e8c5bc1fa1f8081ac338f8212919971dad5853175d65
arm64mingw=2c8012a0b46f9d9e1ab4e8c5bc1fa1f8081ac338f8212919971dad5853175d65
arm64linux=d38338fb5838eca902de781d74ab4f800dcff3d106873c276cbd738ad1039b5a
arm64musl=d38338fb5838eca902de781d74ab4f800dcff3d106873c276cbd738ad1039b5a
arm64glibc=d38338fb5838eca902de781d74ab4f800dcff3d106873c276cbd738ad1039b5a
arm64v1win=2c8012a0b46f9d9e1ab4e8c5bc1fa1f8081ac338f8212919971dad5853175d65
arm64v1mingw=2c8012a0b46f9d9e1ab4e8c5bc1fa1f8081ac338f8212919971dad5853175d65
arm64v1linux=d38338fb5838eca902de781d74ab4f800dcff3d106873c276cbd738ad1039b5a
arm64v1musl=d38338fb5838eca902de781d74ab4f800dcff3d106873c276cbd738ad1039b5a
arm64v1glibc=d38338fb5838eca902de781d74ab4f800dcff3d106873c276cbd738ad1039b5a
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
