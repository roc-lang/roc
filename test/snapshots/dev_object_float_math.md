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
x64mac=8cdfe929d98f75c1013e9fc940e8f951dfe01292d864a1a18b22de9bfb1adc6e
x64win=5b7138fcc29fbf0f09b86a8c0f4cd41eac3f63d4735a2cb0f6391768a2c4b582
x64mingw=5b7138fcc29fbf0f09b86a8c0f4cd41eac3f63d4735a2cb0f6391768a2c4b582
x64freebsd=f40322e27a104bc34a7f68399ece425c22194f7ddee87abdcf5f7d198f9ff045
x64openbsd=a93f4a4dfe7a26950470e5fdb934d39b242765fc21e5f5d62143a835d6f50fa9
x64netbsd=dc51466bf77ee55dc09b08a4f9f99a62babe3bf2a0045882220cdb14bcf64bdf
x64musl=dc51466bf77ee55dc09b08a4f9f99a62babe3bf2a0045882220cdb14bcf64bdf
x64glibc=dc51466bf77ee55dc09b08a4f9f99a62babe3bf2a0045882220cdb14bcf64bdf
x64linux=dc51466bf77ee55dc09b08a4f9f99a62babe3bf2a0045882220cdb14bcf64bdf
x64elf=dc51466bf77ee55dc09b08a4f9f99a62babe3bf2a0045882220cdb14bcf64bdf
x64v1mac=8cdfe929d98f75c1013e9fc940e8f951dfe01292d864a1a18b22de9bfb1adc6e
x64v1win=5b7138fcc29fbf0f09b86a8c0f4cd41eac3f63d4735a2cb0f6391768a2c4b582
x64v1mingw=5b7138fcc29fbf0f09b86a8c0f4cd41eac3f63d4735a2cb0f6391768a2c4b582
x64v1freebsd=f40322e27a104bc34a7f68399ece425c22194f7ddee87abdcf5f7d198f9ff045
x64v1openbsd=a93f4a4dfe7a26950470e5fdb934d39b242765fc21e5f5d62143a835d6f50fa9
x64v1netbsd=dc51466bf77ee55dc09b08a4f9f99a62babe3bf2a0045882220cdb14bcf64bdf
x64v1musl=dc51466bf77ee55dc09b08a4f9f99a62babe3bf2a0045882220cdb14bcf64bdf
x64v1glibc=dc51466bf77ee55dc09b08a4f9f99a62babe3bf2a0045882220cdb14bcf64bdf
x64v1linux=dc51466bf77ee55dc09b08a4f9f99a62babe3bf2a0045882220cdb14bcf64bdf
x64v1elf=dc51466bf77ee55dc09b08a4f9f99a62babe3bf2a0045882220cdb14bcf64bdf
arm64mac=31c9ceb90bcae0248cf8f5cb7bf7f0dd878ac08fc51973407c40e7f34aaed83b
arm64win=1efd2425dc6d01f233166ee19ccb6fbf44998b5aa40a2b7cbf9f24260daaf9da
arm64mingw=1efd2425dc6d01f233166ee19ccb6fbf44998b5aa40a2b7cbf9f24260daaf9da
arm64linux=a511a7166e2cbeda36a4d484cf4bc2cf70fec431d2a5c552f163aba9f73ed150
arm64musl=a511a7166e2cbeda36a4d484cf4bc2cf70fec431d2a5c552f163aba9f73ed150
arm64glibc=a511a7166e2cbeda36a4d484cf4bc2cf70fec431d2a5c552f163aba9f73ed150
arm64v1win=1efd2425dc6d01f233166ee19ccb6fbf44998b5aa40a2b7cbf9f24260daaf9da
arm64v1mingw=1efd2425dc6d01f233166ee19ccb6fbf44998b5aa40a2b7cbf9f24260daaf9da
arm64v1linux=a511a7166e2cbeda36a4d484cf4bc2cf70fec431d2a5c552f163aba9f73ed150
arm64v1musl=a511a7166e2cbeda36a4d484cf4bc2cf70fec431d2a5c552f163aba9f73ed150
arm64v1glibc=a511a7166e2cbeda36a4d484cf4bc2cf70fec431d2a5c552f163aba9f73ed150
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
