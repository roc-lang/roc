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
x64mac=29542e8cd9f5a42bcf5c69bf63d650a3d8bc54c358976e1af721005380ba428d
x64win=5aa46df8744f551baa007214930c33f4519248a0aab85400d3116a48507e9435
x64mingw=5aa46df8744f551baa007214930c33f4519248a0aab85400d3116a48507e9435
x64freebsd=d3e819fd5d9b5d4e034c12138bc0118269cc9a618ca5c5fff258dbc7878e8461
x64openbsd=245bdeec903ed2b22d582dba018e932498d7d36124b3dff1dabe37bc3e22d2ef
x64netbsd=39db9aa7da875f95a324c10019bd2fa2631baf6669eebd26ba2a55c90cd0beda
x64musl=39db9aa7da875f95a324c10019bd2fa2631baf6669eebd26ba2a55c90cd0beda
x64glibc=39db9aa7da875f95a324c10019bd2fa2631baf6669eebd26ba2a55c90cd0beda
x64linux=39db9aa7da875f95a324c10019bd2fa2631baf6669eebd26ba2a55c90cd0beda
x64elf=39db9aa7da875f95a324c10019bd2fa2631baf6669eebd26ba2a55c90cd0beda
x64v1mac=29542e8cd9f5a42bcf5c69bf63d650a3d8bc54c358976e1af721005380ba428d
x64v1win=5aa46df8744f551baa007214930c33f4519248a0aab85400d3116a48507e9435
x64v1mingw=5aa46df8744f551baa007214930c33f4519248a0aab85400d3116a48507e9435
x64v1freebsd=d3e819fd5d9b5d4e034c12138bc0118269cc9a618ca5c5fff258dbc7878e8461
x64v1openbsd=245bdeec903ed2b22d582dba018e932498d7d36124b3dff1dabe37bc3e22d2ef
x64v1netbsd=39db9aa7da875f95a324c10019bd2fa2631baf6669eebd26ba2a55c90cd0beda
x64v1musl=39db9aa7da875f95a324c10019bd2fa2631baf6669eebd26ba2a55c90cd0beda
x64v1glibc=39db9aa7da875f95a324c10019bd2fa2631baf6669eebd26ba2a55c90cd0beda
x64v1linux=39db9aa7da875f95a324c10019bd2fa2631baf6669eebd26ba2a55c90cd0beda
x64v1elf=39db9aa7da875f95a324c10019bd2fa2631baf6669eebd26ba2a55c90cd0beda
arm64mac=b5f24e70a8a1852cc1d9745072f7167826ec072776bb3302db69c8f4b08e06a4
arm64win=649f6ec3ee7b46350ee04a00f93e1c1fda6c7159afce7c9e03094051adedb3b7
arm64mingw=649f6ec3ee7b46350ee04a00f93e1c1fda6c7159afce7c9e03094051adedb3b7
arm64linux=728c834eeb765dd93cbaddc18408f07ef3b3a2c552895f9e5f14fa8cd66064fe
arm64musl=728c834eeb765dd93cbaddc18408f07ef3b3a2c552895f9e5f14fa8cd66064fe
arm64glibc=728c834eeb765dd93cbaddc18408f07ef3b3a2c552895f9e5f14fa8cd66064fe
arm64v1win=649f6ec3ee7b46350ee04a00f93e1c1fda6c7159afce7c9e03094051adedb3b7
arm64v1mingw=649f6ec3ee7b46350ee04a00f93e1c1fda6c7159afce7c9e03094051adedb3b7
arm64v1linux=728c834eeb765dd93cbaddc18408f07ef3b3a2c552895f9e5f14fa8cd66064fe
arm64v1musl=728c834eeb765dd93cbaddc18408f07ef3b3a2c552895f9e5f14fa8cd66064fe
arm64v1glibc=728c834eeb765dd93cbaddc18408f07ef3b3a2c552895f9e5f14fa8cd66064fe
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
