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
x64mac=992aebcb6acf96aa2431f1d26d8726904708493160498db5ce3feeba0a58ab3e
x64win=23134aec2ce4c3b2ff2974378798fb73d31e4b1f35bbf12013f27b300e6c6696
x64mingw=23134aec2ce4c3b2ff2974378798fb73d31e4b1f35bbf12013f27b300e6c6696
x64freebsd=8e0865935fc752645dd9dc8f3b72146a16eaf61077f12262854058553cbb211d
x64openbsd=c9d2279c915b0661980d9f07bff1a5610fcea5aa957d1b807cce6aa815bca434
x64netbsd=e66baac1cb9277385a3bcef366d798ecdd47e8e289ab552c3f40f4ee10f71e14
x64musl=e66baac1cb9277385a3bcef366d798ecdd47e8e289ab552c3f40f4ee10f71e14
x64glibc=e66baac1cb9277385a3bcef366d798ecdd47e8e289ab552c3f40f4ee10f71e14
x64linux=e66baac1cb9277385a3bcef366d798ecdd47e8e289ab552c3f40f4ee10f71e14
x64elf=e66baac1cb9277385a3bcef366d798ecdd47e8e289ab552c3f40f4ee10f71e14
x64v1mac=992aebcb6acf96aa2431f1d26d8726904708493160498db5ce3feeba0a58ab3e
x64v1win=23134aec2ce4c3b2ff2974378798fb73d31e4b1f35bbf12013f27b300e6c6696
x64v1mingw=23134aec2ce4c3b2ff2974378798fb73d31e4b1f35bbf12013f27b300e6c6696
x64v1freebsd=8e0865935fc752645dd9dc8f3b72146a16eaf61077f12262854058553cbb211d
x64v1openbsd=c9d2279c915b0661980d9f07bff1a5610fcea5aa957d1b807cce6aa815bca434
x64v1netbsd=e66baac1cb9277385a3bcef366d798ecdd47e8e289ab552c3f40f4ee10f71e14
x64v1musl=e66baac1cb9277385a3bcef366d798ecdd47e8e289ab552c3f40f4ee10f71e14
x64v1glibc=e66baac1cb9277385a3bcef366d798ecdd47e8e289ab552c3f40f4ee10f71e14
x64v1linux=e66baac1cb9277385a3bcef366d798ecdd47e8e289ab552c3f40f4ee10f71e14
x64v1elf=e66baac1cb9277385a3bcef366d798ecdd47e8e289ab552c3f40f4ee10f71e14
arm64mac=a3e0254c4f0bd8009642345cbe0559a3986275fabff88994f0ef7412c229b5f9
arm64win=9583863c554e91846fbb8abde10f5e9cd0ab70c42c2a85ef39edc0e30100d5d7
arm64mingw=9583863c554e91846fbb8abde10f5e9cd0ab70c42c2a85ef39edc0e30100d5d7
arm64linux=41068cd01bfd5e15fe70454602fc7b3ceb2659798a6acc606dd912e28fc0e269
arm64musl=41068cd01bfd5e15fe70454602fc7b3ceb2659798a6acc606dd912e28fc0e269
arm64glibc=41068cd01bfd5e15fe70454602fc7b3ceb2659798a6acc606dd912e28fc0e269
arm64v1win=9583863c554e91846fbb8abde10f5e9cd0ab70c42c2a85ef39edc0e30100d5d7
arm64v1mingw=9583863c554e91846fbb8abde10f5e9cd0ab70c42c2a85ef39edc0e30100d5d7
arm64v1linux=41068cd01bfd5e15fe70454602fc7b3ceb2659798a6acc606dd912e28fc0e269
arm64v1musl=41068cd01bfd5e15fe70454602fc7b3ceb2659798a6acc606dd912e28fc0e269
arm64v1glibc=41068cd01bfd5e15fe70454602fc7b3ceb2659798a6acc606dd912e28fc0e269
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
