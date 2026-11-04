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
x64mac=60d7fe93bfc427c9ba91a6469a7408c323a0542d040d2172b8b21e41ee5c64a5
x64win=277211f1ec9b6a60827e743d0eafa262530415db1a6f63e122ef2df67a58c7a5
x64mingw=277211f1ec9b6a60827e743d0eafa262530415db1a6f63e122ef2df67a58c7a5
x64freebsd=c0e59ba0b2562fa63457baf5adcb493304b7e9315b0f7271428c0ebe465cfb4b
x64openbsd=68218105da3a6cb0b9877e1d4d6118351e3827249fdbaf887d474f1b47a2cf7d
x64netbsd=2ed14310fb1d315ebf9614e884a635ca0d355e5a15cbf1ee7f89708012a12709
x64musl=2ed14310fb1d315ebf9614e884a635ca0d355e5a15cbf1ee7f89708012a12709
x64glibc=2ed14310fb1d315ebf9614e884a635ca0d355e5a15cbf1ee7f89708012a12709
x64linux=2ed14310fb1d315ebf9614e884a635ca0d355e5a15cbf1ee7f89708012a12709
x64elf=2ed14310fb1d315ebf9614e884a635ca0d355e5a15cbf1ee7f89708012a12709
x64v1mac=60d7fe93bfc427c9ba91a6469a7408c323a0542d040d2172b8b21e41ee5c64a5
x64v1win=277211f1ec9b6a60827e743d0eafa262530415db1a6f63e122ef2df67a58c7a5
x64v1mingw=277211f1ec9b6a60827e743d0eafa262530415db1a6f63e122ef2df67a58c7a5
x64v1freebsd=c0e59ba0b2562fa63457baf5adcb493304b7e9315b0f7271428c0ebe465cfb4b
x64v1openbsd=68218105da3a6cb0b9877e1d4d6118351e3827249fdbaf887d474f1b47a2cf7d
x64v1netbsd=2ed14310fb1d315ebf9614e884a635ca0d355e5a15cbf1ee7f89708012a12709
x64v1musl=2ed14310fb1d315ebf9614e884a635ca0d355e5a15cbf1ee7f89708012a12709
x64v1glibc=2ed14310fb1d315ebf9614e884a635ca0d355e5a15cbf1ee7f89708012a12709
x64v1linux=2ed14310fb1d315ebf9614e884a635ca0d355e5a15cbf1ee7f89708012a12709
x64v1elf=2ed14310fb1d315ebf9614e884a635ca0d355e5a15cbf1ee7f89708012a12709
arm64mac=5cf6f2d8ad04e8980a60d199108d461ffaddf30be495f3e6afe459cb76e3f567
arm64win=753d6bbeb52acc6d6d7e5cc64b02c713ffba11d2e42edd8e160f5d78a1443635
arm64mingw=753d6bbeb52acc6d6d7e5cc64b02c713ffba11d2e42edd8e160f5d78a1443635
arm64linux=9bde3360b5cce96d8d8ae0429606128a48d3585c07aee871f860dc4b31a70267
arm64musl=9bde3360b5cce96d8d8ae0429606128a48d3585c07aee871f860dc4b31a70267
arm64glibc=9bde3360b5cce96d8d8ae0429606128a48d3585c07aee871f860dc4b31a70267
arm64v1win=753d6bbeb52acc6d6d7e5cc64b02c713ffba11d2e42edd8e160f5d78a1443635
arm64v1mingw=753d6bbeb52acc6d6d7e5cc64b02c713ffba11d2e42edd8e160f5d78a1443635
arm64v1linux=9bde3360b5cce96d8d8ae0429606128a48d3585c07aee871f860dc4b31a70267
arm64v1musl=9bde3360b5cce96d8d8ae0429606128a48d3585c07aee871f860dc4b31a70267
arm64v1glibc=9bde3360b5cce96d8d8ae0429606128a48d3585c07aee871f860dc4b31a70267
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
