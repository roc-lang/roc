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
x64mac=831b15d59ebd378a66e292ed96a81e8cb1e786fd682c9dde7b5567d52504f0dc
x64win=252bea62a8d6737eefec36b2b3636a57d44ed05c1362cfb0087ac605f9ba24dd
x64mingw=252bea62a8d6737eefec36b2b3636a57d44ed05c1362cfb0087ac605f9ba24dd
x64freebsd=84d992ca61304bf658cf726c3ae435bd058e47cd450fe323dfe702a2f000979d
x64openbsd=96fb082872d47a019e3d84e34ff284b71c7a65d577a4da591b4eec720dbeda7f
x64netbsd=ce520f3740eaac8c96653fc43d255cec5982bc4d8b0bb07f6271f2be8ef81440
x64musl=ce520f3740eaac8c96653fc43d255cec5982bc4d8b0bb07f6271f2be8ef81440
x64glibc=ce520f3740eaac8c96653fc43d255cec5982bc4d8b0bb07f6271f2be8ef81440
x64linux=ce520f3740eaac8c96653fc43d255cec5982bc4d8b0bb07f6271f2be8ef81440
x64elf=ce520f3740eaac8c96653fc43d255cec5982bc4d8b0bb07f6271f2be8ef81440
x64v1mac=831b15d59ebd378a66e292ed96a81e8cb1e786fd682c9dde7b5567d52504f0dc
x64v1win=252bea62a8d6737eefec36b2b3636a57d44ed05c1362cfb0087ac605f9ba24dd
x64v1mingw=252bea62a8d6737eefec36b2b3636a57d44ed05c1362cfb0087ac605f9ba24dd
x64v1freebsd=84d992ca61304bf658cf726c3ae435bd058e47cd450fe323dfe702a2f000979d
x64v1openbsd=96fb082872d47a019e3d84e34ff284b71c7a65d577a4da591b4eec720dbeda7f
x64v1netbsd=ce520f3740eaac8c96653fc43d255cec5982bc4d8b0bb07f6271f2be8ef81440
x64v1musl=ce520f3740eaac8c96653fc43d255cec5982bc4d8b0bb07f6271f2be8ef81440
x64v1glibc=ce520f3740eaac8c96653fc43d255cec5982bc4d8b0bb07f6271f2be8ef81440
x64v1linux=ce520f3740eaac8c96653fc43d255cec5982bc4d8b0bb07f6271f2be8ef81440
x64v1elf=ce520f3740eaac8c96653fc43d255cec5982bc4d8b0bb07f6271f2be8ef81440
arm64mac=55a250a23b45a48f40c5d9ee051e75078ec05922bd0a407af27813c3ed5712e8
arm64win=69bc080ceb48e2b05ba731df3af53558509daa58877e2c91d7825e46862eb56d
arm64mingw=69bc080ceb48e2b05ba731df3af53558509daa58877e2c91d7825e46862eb56d
arm64linux=d0fa95685756af4e8c57a3853312b8b6ae6abf12cff448993b0875fefcb0804e
arm64musl=d0fa95685756af4e8c57a3853312b8b6ae6abf12cff448993b0875fefcb0804e
arm64glibc=d0fa95685756af4e8c57a3853312b8b6ae6abf12cff448993b0875fefcb0804e
arm64v1win=69bc080ceb48e2b05ba731df3af53558509daa58877e2c91d7825e46862eb56d
arm64v1mingw=69bc080ceb48e2b05ba731df3af53558509daa58877e2c91d7825e46862eb56d
arm64v1linux=d0fa95685756af4e8c57a3853312b8b6ae6abf12cff448993b0875fefcb0804e
arm64v1musl=d0fa95685756af4e8c57a3853312b8b6ae6abf12cff448993b0875fefcb0804e
arm64v1glibc=d0fa95685756af4e8c57a3853312b8b6ae6abf12cff448993b0875fefcb0804e
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
