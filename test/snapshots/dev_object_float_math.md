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
x64mac=b5bb3394e6b87cbdc915e5407642fc554acf54bf412cc742ba9555135e3bd5f1
x64win=6b1cb133f6564099e5a92b2435f7c7ccab236a48d8461b7662f1b6821b0025dd
x64mingw=6b1cb133f6564099e5a92b2435f7c7ccab236a48d8461b7662f1b6821b0025dd
x64freebsd=3b710bd15e1d0a61ff8a901a894e91efadb9cf3bbe59658513bacbb98c55bf6e
x64openbsd=700629a1ebb811bd6bbcbbed68f9a952489d85f14662fcba4025f13592659e01
x64netbsd=f349570e11c3818ec505e7bbf8735c25024caa943b10b3ad7a71b4c4a304c9ac
x64musl=f349570e11c3818ec505e7bbf8735c25024caa943b10b3ad7a71b4c4a304c9ac
x64glibc=f349570e11c3818ec505e7bbf8735c25024caa943b10b3ad7a71b4c4a304c9ac
x64linux=f349570e11c3818ec505e7bbf8735c25024caa943b10b3ad7a71b4c4a304c9ac
x64elf=f349570e11c3818ec505e7bbf8735c25024caa943b10b3ad7a71b4c4a304c9ac
x64v1mac=b5bb3394e6b87cbdc915e5407642fc554acf54bf412cc742ba9555135e3bd5f1
x64v1win=6b1cb133f6564099e5a92b2435f7c7ccab236a48d8461b7662f1b6821b0025dd
x64v1mingw=6b1cb133f6564099e5a92b2435f7c7ccab236a48d8461b7662f1b6821b0025dd
x64v1freebsd=3b710bd15e1d0a61ff8a901a894e91efadb9cf3bbe59658513bacbb98c55bf6e
x64v1openbsd=700629a1ebb811bd6bbcbbed68f9a952489d85f14662fcba4025f13592659e01
x64v1netbsd=f349570e11c3818ec505e7bbf8735c25024caa943b10b3ad7a71b4c4a304c9ac
x64v1musl=f349570e11c3818ec505e7bbf8735c25024caa943b10b3ad7a71b4c4a304c9ac
x64v1glibc=f349570e11c3818ec505e7bbf8735c25024caa943b10b3ad7a71b4c4a304c9ac
x64v1linux=f349570e11c3818ec505e7bbf8735c25024caa943b10b3ad7a71b4c4a304c9ac
x64v1elf=f349570e11c3818ec505e7bbf8735c25024caa943b10b3ad7a71b4c4a304c9ac
arm64mac=37126aa09bcbabdb4dc4dedba8ef6626ca952e2aca5f67f4ca8bd0cab560b9aa
arm64win=5aa45cbd708edf917eba563ed18b5ed78b31ad053f8588acf479af7d2f659675
arm64mingw=5aa45cbd708edf917eba563ed18b5ed78b31ad053f8588acf479af7d2f659675
arm64linux=7135c32d3ff27463233295c03adb60baa55cd7cf0443771dae343d51ca4c86cd
arm64musl=7135c32d3ff27463233295c03adb60baa55cd7cf0443771dae343d51ca4c86cd
arm64glibc=7135c32d3ff27463233295c03adb60baa55cd7cf0443771dae343d51ca4c86cd
arm64v1win=5aa45cbd708edf917eba563ed18b5ed78b31ad053f8588acf479af7d2f659675
arm64v1mingw=5aa45cbd708edf917eba563ed18b5ed78b31ad053f8588acf479af7d2f659675
arm64v1linux=7135c32d3ff27463233295c03adb60baa55cd7cf0443771dae343d51ca4c86cd
arm64v1musl=7135c32d3ff27463233295c03adb60baa55cd7cf0443771dae343d51ca4c86cd
arm64v1glibc=7135c32d3ff27463233295c03adb60baa55cd7cf0443771dae343d51ca4c86cd
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
