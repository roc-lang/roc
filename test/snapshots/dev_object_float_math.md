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
x64mac=39c62a7538c70afb6a9b7d56af6e42f260210bfab53cb6d9b25fed2469aaba9d
x64win=a00aad72e34a993f7d9ac374ed9e4e6730394daf1996507f786e1d10092edf8d
x64mingw=a00aad72e34a993f7d9ac374ed9e4e6730394daf1996507f786e1d10092edf8d
x64freebsd=d9347d8c38f967d3da2858fa1c66a9bb0bc02df4bb95d6d23c389a38d476537d
x64openbsd=26a507fc8d1393f74cc1b3b60e1e82868fb9aa09d13d1f746058c7eeefba472e
x64netbsd=3a99c730844e11642c2d2032fa687e0c6c919ff76a2bbe9ae13f7f3c5924a685
x64musl=3a99c730844e11642c2d2032fa687e0c6c919ff76a2bbe9ae13f7f3c5924a685
x64glibc=3a99c730844e11642c2d2032fa687e0c6c919ff76a2bbe9ae13f7f3c5924a685
x64linux=3a99c730844e11642c2d2032fa687e0c6c919ff76a2bbe9ae13f7f3c5924a685
x64elf=3a99c730844e11642c2d2032fa687e0c6c919ff76a2bbe9ae13f7f3c5924a685
x64v1mac=39c62a7538c70afb6a9b7d56af6e42f260210bfab53cb6d9b25fed2469aaba9d
x64v1win=a00aad72e34a993f7d9ac374ed9e4e6730394daf1996507f786e1d10092edf8d
x64v1mingw=a00aad72e34a993f7d9ac374ed9e4e6730394daf1996507f786e1d10092edf8d
x64v1freebsd=d9347d8c38f967d3da2858fa1c66a9bb0bc02df4bb95d6d23c389a38d476537d
x64v1openbsd=26a507fc8d1393f74cc1b3b60e1e82868fb9aa09d13d1f746058c7eeefba472e
x64v1netbsd=3a99c730844e11642c2d2032fa687e0c6c919ff76a2bbe9ae13f7f3c5924a685
x64v1musl=3a99c730844e11642c2d2032fa687e0c6c919ff76a2bbe9ae13f7f3c5924a685
x64v1glibc=3a99c730844e11642c2d2032fa687e0c6c919ff76a2bbe9ae13f7f3c5924a685
x64v1linux=3a99c730844e11642c2d2032fa687e0c6c919ff76a2bbe9ae13f7f3c5924a685
x64v1elf=3a99c730844e11642c2d2032fa687e0c6c919ff76a2bbe9ae13f7f3c5924a685
arm64mac=241c759ecc89e661fad3bfaf5789e5ccc7e9eec307ae637f0ae30fcf91fece7b
arm64win=1e0fd2527f76f5937b7686e0b40b66615be40f3f6b2a129cf11ff1d0848fa4a0
arm64mingw=1e0fd2527f76f5937b7686e0b40b66615be40f3f6b2a129cf11ff1d0848fa4a0
arm64linux=98e8b0b717c7235f4edaf2c14b7a4feed2751213da2a7840063127cbc6c8527f
arm64musl=98e8b0b717c7235f4edaf2c14b7a4feed2751213da2a7840063127cbc6c8527f
arm64glibc=98e8b0b717c7235f4edaf2c14b7a4feed2751213da2a7840063127cbc6c8527f
arm64v1win=1e0fd2527f76f5937b7686e0b40b66615be40f3f6b2a129cf11ff1d0848fa4a0
arm64v1mingw=1e0fd2527f76f5937b7686e0b40b66615be40f3f6b2a129cf11ff1d0848fa4a0
arm64v1linux=98e8b0b717c7235f4edaf2c14b7a4feed2751213da2a7840063127cbc6c8527f
arm64v1musl=98e8b0b717c7235f4edaf2c14b7a4feed2751213da2a7840063127cbc6c8527f
arm64v1glibc=98e8b0b717c7235f4edaf2c14b7a4feed2751213da2a7840063127cbc6c8527f
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
