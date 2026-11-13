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
x64mac=54fe26e59921accb5923566dd97e55085dbc1359741e6d6eec03687d39fbd81d
x64win=4d441ce4b1d3aff7bb570acf78fb30986940e2ae98cd99cdeda6803ecadca447
x64mingw=4d441ce4b1d3aff7bb570acf78fb30986940e2ae98cd99cdeda6803ecadca447
x64freebsd=bbe0d7c6b1c9d2d02a23bec62a7ae3c35477b7992e478fae105ddfa30149b772
x64openbsd=6f8b21c94100e1e89783b32d72a37f52f5a62732266bcd2147a769d49a632d01
x64netbsd=4976a380ba7be8024e067ce30eb17e79d7176f06dc85c1bf24122ba986fef972
x64musl=4976a380ba7be8024e067ce30eb17e79d7176f06dc85c1bf24122ba986fef972
x64glibc=4976a380ba7be8024e067ce30eb17e79d7176f06dc85c1bf24122ba986fef972
x64linux=4976a380ba7be8024e067ce30eb17e79d7176f06dc85c1bf24122ba986fef972
x64elf=4976a380ba7be8024e067ce30eb17e79d7176f06dc85c1bf24122ba986fef972
x64v1mac=54fe26e59921accb5923566dd97e55085dbc1359741e6d6eec03687d39fbd81d
x64v1win=4d441ce4b1d3aff7bb570acf78fb30986940e2ae98cd99cdeda6803ecadca447
x64v1mingw=4d441ce4b1d3aff7bb570acf78fb30986940e2ae98cd99cdeda6803ecadca447
x64v1freebsd=bbe0d7c6b1c9d2d02a23bec62a7ae3c35477b7992e478fae105ddfa30149b772
x64v1openbsd=6f8b21c94100e1e89783b32d72a37f52f5a62732266bcd2147a769d49a632d01
x64v1netbsd=4976a380ba7be8024e067ce30eb17e79d7176f06dc85c1bf24122ba986fef972
x64v1musl=4976a380ba7be8024e067ce30eb17e79d7176f06dc85c1bf24122ba986fef972
x64v1glibc=4976a380ba7be8024e067ce30eb17e79d7176f06dc85c1bf24122ba986fef972
x64v1linux=4976a380ba7be8024e067ce30eb17e79d7176f06dc85c1bf24122ba986fef972
x64v1elf=4976a380ba7be8024e067ce30eb17e79d7176f06dc85c1bf24122ba986fef972
arm64mac=b9b83f72cc34e9ab5fcc3f19a5e1655f2985244907ad3bc24ba3937d472a0f0d
arm64win=67bc187fb5768dfb958b0f4b3edb0e1453c89123759de5abfb019cab6ce2e5f5
arm64mingw=67bc187fb5768dfb958b0f4b3edb0e1453c89123759de5abfb019cab6ce2e5f5
arm64linux=cddaf72ffbffd9c7c8b47b1914b428dedaf71ad47ecee4abdd2aaba648160b69
arm64musl=cddaf72ffbffd9c7c8b47b1914b428dedaf71ad47ecee4abdd2aaba648160b69
arm64glibc=cddaf72ffbffd9c7c8b47b1914b428dedaf71ad47ecee4abdd2aaba648160b69
arm64v1win=67bc187fb5768dfb958b0f4b3edb0e1453c89123759de5abfb019cab6ce2e5f5
arm64v1mingw=67bc187fb5768dfb958b0f4b3edb0e1453c89123759de5abfb019cab6ce2e5f5
arm64v1linux=cddaf72ffbffd9c7c8b47b1914b428dedaf71ad47ecee4abdd2aaba648160b69
arm64v1musl=cddaf72ffbffd9c7c8b47b1914b428dedaf71ad47ecee4abdd2aaba648160b69
arm64v1glibc=cddaf72ffbffd9c7c8b47b1914b428dedaf71ad47ecee4abdd2aaba648160b69
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
