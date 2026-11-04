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
x64mac=38cb7d8d911e349a661adc4c9719a6273efb0ab5682590af0e9d2ef00d9e980c
x64win=0aeea8fa98d944cb4ca9de229b151f3e4657aa6d218384cdbd2f952a4f463a50
x64mingw=0aeea8fa98d944cb4ca9de229b151f3e4657aa6d218384cdbd2f952a4f463a50
x64freebsd=a569e846267a38e233bfef41384ae3a011bbec4bc3e39a13d69fab73f1084f69
x64openbsd=948ba1692f254c93a23bf64c72b0224547c742d7fff96a0c1dc51d3e18c8c395
x64netbsd=da4ee30a465fcd021bf2885835f45f754d4693d66bab980c0d74370d6fcc1216
x64musl=da4ee30a465fcd021bf2885835f45f754d4693d66bab980c0d74370d6fcc1216
x64glibc=da4ee30a465fcd021bf2885835f45f754d4693d66bab980c0d74370d6fcc1216
x64linux=da4ee30a465fcd021bf2885835f45f754d4693d66bab980c0d74370d6fcc1216
x64elf=da4ee30a465fcd021bf2885835f45f754d4693d66bab980c0d74370d6fcc1216
x64v1mac=38cb7d8d911e349a661adc4c9719a6273efb0ab5682590af0e9d2ef00d9e980c
x64v1win=0aeea8fa98d944cb4ca9de229b151f3e4657aa6d218384cdbd2f952a4f463a50
x64v1mingw=0aeea8fa98d944cb4ca9de229b151f3e4657aa6d218384cdbd2f952a4f463a50
x64v1freebsd=a569e846267a38e233bfef41384ae3a011bbec4bc3e39a13d69fab73f1084f69
x64v1openbsd=948ba1692f254c93a23bf64c72b0224547c742d7fff96a0c1dc51d3e18c8c395
x64v1netbsd=da4ee30a465fcd021bf2885835f45f754d4693d66bab980c0d74370d6fcc1216
x64v1musl=da4ee30a465fcd021bf2885835f45f754d4693d66bab980c0d74370d6fcc1216
x64v1glibc=da4ee30a465fcd021bf2885835f45f754d4693d66bab980c0d74370d6fcc1216
x64v1linux=da4ee30a465fcd021bf2885835f45f754d4693d66bab980c0d74370d6fcc1216
x64v1elf=da4ee30a465fcd021bf2885835f45f754d4693d66bab980c0d74370d6fcc1216
arm64mac=ac8939e0a42a44551450ad215c83785f5c79c06ead4ca0ba0f5b54479fe5e32d
arm64win=de7cf6ea04d229b89c1ba0547cb8a8d0e5dbe56c2cef67acab2b00d57188961b
arm64mingw=de7cf6ea04d229b89c1ba0547cb8a8d0e5dbe56c2cef67acab2b00d57188961b
arm64linux=ad4b9351e15dcd8f5c8fd9916dbba60e86742a6bdcbc93e0a20d209435aed575
arm64musl=ad4b9351e15dcd8f5c8fd9916dbba60e86742a6bdcbc93e0a20d209435aed575
arm64glibc=ad4b9351e15dcd8f5c8fd9916dbba60e86742a6bdcbc93e0a20d209435aed575
arm64v1win=de7cf6ea04d229b89c1ba0547cb8a8d0e5dbe56c2cef67acab2b00d57188961b
arm64v1mingw=de7cf6ea04d229b89c1ba0547cb8a8d0e5dbe56c2cef67acab2b00d57188961b
arm64v1linux=ad4b9351e15dcd8f5c8fd9916dbba60e86742a6bdcbc93e0a20d209435aed575
arm64v1musl=ad4b9351e15dcd8f5c8fd9916dbba60e86742a6bdcbc93e0a20d209435aed575
arm64v1glibc=ad4b9351e15dcd8f5c8fd9916dbba60e86742a6bdcbc93e0a20d209435aed575
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
