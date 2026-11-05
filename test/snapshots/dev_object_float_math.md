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
x64mac=ea9595c23bc2d18cfedefa404ebb3ae1d892b0b7814e5384dad826e25f193cf9
x64win=e2cbd1659c8ba906617d914c89b6d4f4871ce6dd74cf7bd1bc586aaac2197798
x64mingw=e2cbd1659c8ba906617d914c89b6d4f4871ce6dd74cf7bd1bc586aaac2197798
x64freebsd=81c3be35b28cbeb2c4d9efd0913e592ba3dbdd84076d1ff3cfd9f177cd79e9e5
x64openbsd=ee5e97aa60d36bb6758f8170b6e231680aecba30bc152766dd36fc7952c2a3f2
x64netbsd=a2daa14c37e6bde272a6e50a43d10f943627e0fd1ecdd07e3489b9c8e6b04f7c
x64musl=a2daa14c37e6bde272a6e50a43d10f943627e0fd1ecdd07e3489b9c8e6b04f7c
x64glibc=a2daa14c37e6bde272a6e50a43d10f943627e0fd1ecdd07e3489b9c8e6b04f7c
x64linux=a2daa14c37e6bde272a6e50a43d10f943627e0fd1ecdd07e3489b9c8e6b04f7c
x64elf=a2daa14c37e6bde272a6e50a43d10f943627e0fd1ecdd07e3489b9c8e6b04f7c
x64v1mac=ea9595c23bc2d18cfedefa404ebb3ae1d892b0b7814e5384dad826e25f193cf9
x64v1win=e2cbd1659c8ba906617d914c89b6d4f4871ce6dd74cf7bd1bc586aaac2197798
x64v1mingw=e2cbd1659c8ba906617d914c89b6d4f4871ce6dd74cf7bd1bc586aaac2197798
x64v1freebsd=81c3be35b28cbeb2c4d9efd0913e592ba3dbdd84076d1ff3cfd9f177cd79e9e5
x64v1openbsd=ee5e97aa60d36bb6758f8170b6e231680aecba30bc152766dd36fc7952c2a3f2
x64v1netbsd=a2daa14c37e6bde272a6e50a43d10f943627e0fd1ecdd07e3489b9c8e6b04f7c
x64v1musl=a2daa14c37e6bde272a6e50a43d10f943627e0fd1ecdd07e3489b9c8e6b04f7c
x64v1glibc=a2daa14c37e6bde272a6e50a43d10f943627e0fd1ecdd07e3489b9c8e6b04f7c
x64v1linux=a2daa14c37e6bde272a6e50a43d10f943627e0fd1ecdd07e3489b9c8e6b04f7c
x64v1elf=a2daa14c37e6bde272a6e50a43d10f943627e0fd1ecdd07e3489b9c8e6b04f7c
arm64mac=5f977820bfa67b881797d325f1f695b613de38d6ad97f0eae7f74d249f3acf71
arm64win=4db0eb0dd6353c38ce3289c8a5f4a47e296b56968b69f3e13c577431bd08efb1
arm64mingw=4db0eb0dd6353c38ce3289c8a5f4a47e296b56968b69f3e13c577431bd08efb1
arm64linux=e3c638971cebd5068a2fa88be014d5b67ed9b56c54136617f48c6df32c039846
arm64musl=e3c638971cebd5068a2fa88be014d5b67ed9b56c54136617f48c6df32c039846
arm64glibc=e3c638971cebd5068a2fa88be014d5b67ed9b56c54136617f48c6df32c039846
arm64v1win=4db0eb0dd6353c38ce3289c8a5f4a47e296b56968b69f3e13c577431bd08efb1
arm64v1mingw=4db0eb0dd6353c38ce3289c8a5f4a47e296b56968b69f3e13c577431bd08efb1
arm64v1linux=e3c638971cebd5068a2fa88be014d5b67ed9b56c54136617f48c6df32c039846
arm64v1musl=e3c638971cebd5068a2fa88be014d5b67ed9b56c54136617f48c6df32c039846
arm64v1glibc=e3c638971cebd5068a2fa88be014d5b67ed9b56c54136617f48c6df32c039846
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
