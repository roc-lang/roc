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
x64mac=e8fd7f831c91399f21a004088a0628fc80bdd445b9b679180fb022869d1a1886
x64win=1d35f4e973374cf8fd42090fb0fb6f16e100aff1afc053c4b899e9dd5cdbed82
x64mingw=1d35f4e973374cf8fd42090fb0fb6f16e100aff1afc053c4b899e9dd5cdbed82
x64freebsd=1747fe58bbbc104a43f4ace61f2ff64d119cc0d7a62f2cec348ecb1ae087a7ce
x64openbsd=dee2e47075f630a88b7dcbb1824639f5398604b1413bcdd821ae45d2c6702da6
x64netbsd=dcc9a4235c581469f1862e9dd92cac798ea212a1e2ec18552b518a9385238899
x64musl=dcc9a4235c581469f1862e9dd92cac798ea212a1e2ec18552b518a9385238899
x64glibc=dcc9a4235c581469f1862e9dd92cac798ea212a1e2ec18552b518a9385238899
x64linux=dcc9a4235c581469f1862e9dd92cac798ea212a1e2ec18552b518a9385238899
x64elf=dcc9a4235c581469f1862e9dd92cac798ea212a1e2ec18552b518a9385238899
x64v1mac=e8fd7f831c91399f21a004088a0628fc80bdd445b9b679180fb022869d1a1886
x64v1win=1d35f4e973374cf8fd42090fb0fb6f16e100aff1afc053c4b899e9dd5cdbed82
x64v1mingw=1d35f4e973374cf8fd42090fb0fb6f16e100aff1afc053c4b899e9dd5cdbed82
x64v1freebsd=1747fe58bbbc104a43f4ace61f2ff64d119cc0d7a62f2cec348ecb1ae087a7ce
x64v1openbsd=dee2e47075f630a88b7dcbb1824639f5398604b1413bcdd821ae45d2c6702da6
x64v1netbsd=dcc9a4235c581469f1862e9dd92cac798ea212a1e2ec18552b518a9385238899
x64v1musl=dcc9a4235c581469f1862e9dd92cac798ea212a1e2ec18552b518a9385238899
x64v1glibc=dcc9a4235c581469f1862e9dd92cac798ea212a1e2ec18552b518a9385238899
x64v1linux=dcc9a4235c581469f1862e9dd92cac798ea212a1e2ec18552b518a9385238899
x64v1elf=dcc9a4235c581469f1862e9dd92cac798ea212a1e2ec18552b518a9385238899
arm64mac=8836a7b56f7eb50df1d50d6c5735e4e91cc1aa52c8d733a74aee2d02c6a04e47
arm64win=7528a21933bc7491fe3d1b2b2b2bd3ffdf761237d08b31ad5d23da1a21ac9d8b
arm64mingw=7528a21933bc7491fe3d1b2b2b2bd3ffdf761237d08b31ad5d23da1a21ac9d8b
arm64linux=b90369501a741be0813657f3f640c3b9b6a361bed60ccf8d8351d6cc6247209b
arm64musl=b90369501a741be0813657f3f640c3b9b6a361bed60ccf8d8351d6cc6247209b
arm64glibc=b90369501a741be0813657f3f640c3b9b6a361bed60ccf8d8351d6cc6247209b
arm64v1win=7528a21933bc7491fe3d1b2b2b2bd3ffdf761237d08b31ad5d23da1a21ac9d8b
arm64v1mingw=7528a21933bc7491fe3d1b2b2b2bd3ffdf761237d08b31ad5d23da1a21ac9d8b
arm64v1linux=b90369501a741be0813657f3f640c3b9b6a361bed60ccf8d8351d6cc6247209b
arm64v1musl=b90369501a741be0813657f3f640c3b9b6a361bed60ccf8d8351d6cc6247209b
arm64v1glibc=b90369501a741be0813657f3f640c3b9b6a361bed60ccf8d8351d6cc6247209b
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
