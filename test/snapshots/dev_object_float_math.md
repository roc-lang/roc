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
x64mac=c44cbccd61531811909c9c1773b34968bc688aee3dffc2b035ad66751c5bd904
x64win=2f64e8eb66fb5761060f62455f4057dc17ecaa0b2c9a5a32739f43701576433f
x64mingw=2f64e8eb66fb5761060f62455f4057dc17ecaa0b2c9a5a32739f43701576433f
x64freebsd=9abac239aea7854c1653394d0fe3aec4ac7bc84a28f1b60ff3ebf8048db20f02
x64openbsd=11e5a0e9dbb9021ebfd8f0fe6b722d71edcd3d258ff76da775a2cf035e2b8b67
x64netbsd=b7c4c9771fb2623262f625d4ed9dd4ede16e643b8b6af5167e82bab8280a6310
x64musl=b7c4c9771fb2623262f625d4ed9dd4ede16e643b8b6af5167e82bab8280a6310
x64glibc=b7c4c9771fb2623262f625d4ed9dd4ede16e643b8b6af5167e82bab8280a6310
x64linux=b7c4c9771fb2623262f625d4ed9dd4ede16e643b8b6af5167e82bab8280a6310
x64elf=b7c4c9771fb2623262f625d4ed9dd4ede16e643b8b6af5167e82bab8280a6310
x64v1mac=c44cbccd61531811909c9c1773b34968bc688aee3dffc2b035ad66751c5bd904
x64v1win=2f64e8eb66fb5761060f62455f4057dc17ecaa0b2c9a5a32739f43701576433f
x64v1mingw=2f64e8eb66fb5761060f62455f4057dc17ecaa0b2c9a5a32739f43701576433f
x64v1freebsd=9abac239aea7854c1653394d0fe3aec4ac7bc84a28f1b60ff3ebf8048db20f02
x64v1openbsd=11e5a0e9dbb9021ebfd8f0fe6b722d71edcd3d258ff76da775a2cf035e2b8b67
x64v1netbsd=b7c4c9771fb2623262f625d4ed9dd4ede16e643b8b6af5167e82bab8280a6310
x64v1musl=b7c4c9771fb2623262f625d4ed9dd4ede16e643b8b6af5167e82bab8280a6310
x64v1glibc=b7c4c9771fb2623262f625d4ed9dd4ede16e643b8b6af5167e82bab8280a6310
x64v1linux=b7c4c9771fb2623262f625d4ed9dd4ede16e643b8b6af5167e82bab8280a6310
x64v1elf=b7c4c9771fb2623262f625d4ed9dd4ede16e643b8b6af5167e82bab8280a6310
arm64mac=f1807a62244df33b73de010fa9c18e2b7d11ea32bc9f9f21b3c386fb64e3d49e
arm64win=0354da90309d574739662e22984d9f53f4b5b12c45e59de939063170ee595bb3
arm64mingw=0354da90309d574739662e22984d9f53f4b5b12c45e59de939063170ee595bb3
arm64linux=c71a032f89c03d90dd250a3a67f9b6f7dee7bb0b38647591bc60481ef55b5abd
arm64musl=c71a032f89c03d90dd250a3a67f9b6f7dee7bb0b38647591bc60481ef55b5abd
arm64glibc=c71a032f89c03d90dd250a3a67f9b6f7dee7bb0b38647591bc60481ef55b5abd
arm64v1win=0354da90309d574739662e22984d9f53f4b5b12c45e59de939063170ee595bb3
arm64v1mingw=0354da90309d574739662e22984d9f53f4b5b12c45e59de939063170ee595bb3
arm64v1linux=c71a032f89c03d90dd250a3a67f9b6f7dee7bb0b38647591bc60481ef55b5abd
arm64v1musl=c71a032f89c03d90dd250a3a67f9b6f7dee7bb0b38647591bc60481ef55b5abd
arm64v1glibc=c71a032f89c03d90dd250a3a67f9b6f7dee7bb0b38647591bc60481ef55b5abd
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
