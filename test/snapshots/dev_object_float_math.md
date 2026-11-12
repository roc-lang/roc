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
x64mac=4f49662bd30d5b33ca2832e2420ba28b084dc3ce51a4b5b2c2b4e17cfb8701e8
x64win=acfcef1a556a4c9bf2fd4fe0f49f9710446c59f7d6313d8e0a270c014420a422
x64mingw=acfcef1a556a4c9bf2fd4fe0f49f9710446c59f7d6313d8e0a270c014420a422
x64freebsd=4eff2f1422183b5caa638b4b90a56c1d84518a725134d569f5940859e3f7e8ed
x64openbsd=8870e9d920345a4c997e36223eacf33a058aa9b39dc6a71ecc393b8cb6735956
x64netbsd=2dd6aad19dd5c1aeda1c539e3eedff34e2d43cb029998dc61df0dac50df013e0
x64musl=2dd6aad19dd5c1aeda1c539e3eedff34e2d43cb029998dc61df0dac50df013e0
x64glibc=2dd6aad19dd5c1aeda1c539e3eedff34e2d43cb029998dc61df0dac50df013e0
x64linux=2dd6aad19dd5c1aeda1c539e3eedff34e2d43cb029998dc61df0dac50df013e0
x64elf=2dd6aad19dd5c1aeda1c539e3eedff34e2d43cb029998dc61df0dac50df013e0
x64v1mac=4f49662bd30d5b33ca2832e2420ba28b084dc3ce51a4b5b2c2b4e17cfb8701e8
x64v1win=acfcef1a556a4c9bf2fd4fe0f49f9710446c59f7d6313d8e0a270c014420a422
x64v1mingw=acfcef1a556a4c9bf2fd4fe0f49f9710446c59f7d6313d8e0a270c014420a422
x64v1freebsd=4eff2f1422183b5caa638b4b90a56c1d84518a725134d569f5940859e3f7e8ed
x64v1openbsd=8870e9d920345a4c997e36223eacf33a058aa9b39dc6a71ecc393b8cb6735956
x64v1netbsd=2dd6aad19dd5c1aeda1c539e3eedff34e2d43cb029998dc61df0dac50df013e0
x64v1musl=2dd6aad19dd5c1aeda1c539e3eedff34e2d43cb029998dc61df0dac50df013e0
x64v1glibc=2dd6aad19dd5c1aeda1c539e3eedff34e2d43cb029998dc61df0dac50df013e0
x64v1linux=2dd6aad19dd5c1aeda1c539e3eedff34e2d43cb029998dc61df0dac50df013e0
x64v1elf=2dd6aad19dd5c1aeda1c539e3eedff34e2d43cb029998dc61df0dac50df013e0
arm64mac=f258d9dca60571039c54506500d01f42c9cf23f08470d1a04e2cd8e69d0f4ad8
arm64win=924e481fd01085b5b43b8f5e86cfc8e2b12ffbbafeb8f4e426796e97d841a3b6
arm64mingw=924e481fd01085b5b43b8f5e86cfc8e2b12ffbbafeb8f4e426796e97d841a3b6
arm64linux=b5db5e0fb65e00251ae2604a172564dd38132ef39f80182129f9b83671106cdd
arm64musl=b5db5e0fb65e00251ae2604a172564dd38132ef39f80182129f9b83671106cdd
arm64glibc=b5db5e0fb65e00251ae2604a172564dd38132ef39f80182129f9b83671106cdd
arm64v1win=924e481fd01085b5b43b8f5e86cfc8e2b12ffbbafeb8f4e426796e97d841a3b6
arm64v1mingw=924e481fd01085b5b43b8f5e86cfc8e2b12ffbbafeb8f4e426796e97d841a3b6
arm64v1linux=b5db5e0fb65e00251ae2604a172564dd38132ef39f80182129f9b83671106cdd
arm64v1musl=b5db5e0fb65e00251ae2604a172564dd38132ef39f80182129f9b83671106cdd
arm64v1glibc=b5db5e0fb65e00251ae2604a172564dd38132ef39f80182129f9b83671106cdd
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
