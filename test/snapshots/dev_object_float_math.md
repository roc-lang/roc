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
x64mac=36d3d4dbd7bcd60e47b75005cdc1f7d1eb47ff2259bfb7340995ce9909dbbfc3
x64win=3f0baa2b16ae46b9d32ccb0459680d2e84af70839279a77b2675fbe775f35f69
x64mingw=3f0baa2b16ae46b9d32ccb0459680d2e84af70839279a77b2675fbe775f35f69
x64freebsd=e13bb65904c3867f3e13172e6d059fd428644c63ad158b9a3045ab71d5e8551d
x64openbsd=379adba75baeed88cb76bc1a8dfd90bd4cca6f930ba3c7a82d0990a0fbd09193
x64netbsd=76e03697874e0a2919e1cc5680a1a8f9cecbfb6facefb90b34cf05de3b879d35
x64musl=76e03697874e0a2919e1cc5680a1a8f9cecbfb6facefb90b34cf05de3b879d35
x64glibc=76e03697874e0a2919e1cc5680a1a8f9cecbfb6facefb90b34cf05de3b879d35
x64linux=76e03697874e0a2919e1cc5680a1a8f9cecbfb6facefb90b34cf05de3b879d35
x64elf=76e03697874e0a2919e1cc5680a1a8f9cecbfb6facefb90b34cf05de3b879d35
x64v1mac=36d3d4dbd7bcd60e47b75005cdc1f7d1eb47ff2259bfb7340995ce9909dbbfc3
x64v1win=3f0baa2b16ae46b9d32ccb0459680d2e84af70839279a77b2675fbe775f35f69
x64v1mingw=3f0baa2b16ae46b9d32ccb0459680d2e84af70839279a77b2675fbe775f35f69
x64v1freebsd=e13bb65904c3867f3e13172e6d059fd428644c63ad158b9a3045ab71d5e8551d
x64v1openbsd=379adba75baeed88cb76bc1a8dfd90bd4cca6f930ba3c7a82d0990a0fbd09193
x64v1netbsd=76e03697874e0a2919e1cc5680a1a8f9cecbfb6facefb90b34cf05de3b879d35
x64v1musl=76e03697874e0a2919e1cc5680a1a8f9cecbfb6facefb90b34cf05de3b879d35
x64v1glibc=76e03697874e0a2919e1cc5680a1a8f9cecbfb6facefb90b34cf05de3b879d35
x64v1linux=76e03697874e0a2919e1cc5680a1a8f9cecbfb6facefb90b34cf05de3b879d35
x64v1elf=76e03697874e0a2919e1cc5680a1a8f9cecbfb6facefb90b34cf05de3b879d35
arm64mac=91c788703a5b954cf33208498c92cc738ae67c9abaeb412720efd1ddd67f918c
arm64win=c3720be53d7c3ce43f8089bedd95819b35d3a30c3a6e81100efe71df963e6fa0
arm64mingw=c3720be53d7c3ce43f8089bedd95819b35d3a30c3a6e81100efe71df963e6fa0
arm64linux=03bd67592afd2835569a890b376bfeae1e66a164c79b1b16d1556a706571826b
arm64musl=03bd67592afd2835569a890b376bfeae1e66a164c79b1b16d1556a706571826b
arm64glibc=03bd67592afd2835569a890b376bfeae1e66a164c79b1b16d1556a706571826b
arm64v1win=c3720be53d7c3ce43f8089bedd95819b35d3a30c3a6e81100efe71df963e6fa0
arm64v1mingw=c3720be53d7c3ce43f8089bedd95819b35d3a30c3a6e81100efe71df963e6fa0
arm64v1linux=03bd67592afd2835569a890b376bfeae1e66a164c79b1b16d1556a706571826b
arm64v1musl=03bd67592afd2835569a890b376bfeae1e66a164c79b1b16d1556a706571826b
arm64v1glibc=03bd67592afd2835569a890b376bfeae1e66a164c79b1b16d1556a706571826b
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
