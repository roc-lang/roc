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
x64mac=875878b08c14a449dac0273941d9418f828c18c4c8bf7bff8d0a46a40e226363
x64win=dc8ac70ddeace9c8e20749e6b62b0dc79b8d17217b4128f3ab1636dc612bba7a
x64mingw=dc8ac70ddeace9c8e20749e6b62b0dc79b8d17217b4128f3ab1636dc612bba7a
x64freebsd=b041477504a4b3d5c54b14681e3e38d30c7f45be8067f0d3c1519e0338cd5e35
x64openbsd=0064d6174a85b3c641353109f4ec0f5519a55c9cf44822c18c172c28ebc96f89
x64netbsd=f3e4c1fff71ca11a9d08ea5704ee65db969b2c4813565af946e54994ea2c052f
x64musl=f3e4c1fff71ca11a9d08ea5704ee65db969b2c4813565af946e54994ea2c052f
x64glibc=f3e4c1fff71ca11a9d08ea5704ee65db969b2c4813565af946e54994ea2c052f
x64linux=f3e4c1fff71ca11a9d08ea5704ee65db969b2c4813565af946e54994ea2c052f
x64elf=f3e4c1fff71ca11a9d08ea5704ee65db969b2c4813565af946e54994ea2c052f
x64v1mac=875878b08c14a449dac0273941d9418f828c18c4c8bf7bff8d0a46a40e226363
x64v1win=dc8ac70ddeace9c8e20749e6b62b0dc79b8d17217b4128f3ab1636dc612bba7a
x64v1mingw=dc8ac70ddeace9c8e20749e6b62b0dc79b8d17217b4128f3ab1636dc612bba7a
x64v1freebsd=b041477504a4b3d5c54b14681e3e38d30c7f45be8067f0d3c1519e0338cd5e35
x64v1openbsd=0064d6174a85b3c641353109f4ec0f5519a55c9cf44822c18c172c28ebc96f89
x64v1netbsd=f3e4c1fff71ca11a9d08ea5704ee65db969b2c4813565af946e54994ea2c052f
x64v1musl=f3e4c1fff71ca11a9d08ea5704ee65db969b2c4813565af946e54994ea2c052f
x64v1glibc=f3e4c1fff71ca11a9d08ea5704ee65db969b2c4813565af946e54994ea2c052f
x64v1linux=f3e4c1fff71ca11a9d08ea5704ee65db969b2c4813565af946e54994ea2c052f
x64v1elf=f3e4c1fff71ca11a9d08ea5704ee65db969b2c4813565af946e54994ea2c052f
arm64mac=4ea0dff80643c77c196ade4734f64ded541e10fc0a0a8255bae88aa374e3d770
arm64win=7db68a1f165b4bdab997abf7d91e00230c9112b4e75419a7564fa1dc44780716
arm64mingw=7db68a1f165b4bdab997abf7d91e00230c9112b4e75419a7564fa1dc44780716
arm64linux=4d31ddab46a215c9c563c7f93e64700e7460f6fb8755886dd4b606b8510a026b
arm64musl=4d31ddab46a215c9c563c7f93e64700e7460f6fb8755886dd4b606b8510a026b
arm64glibc=4d31ddab46a215c9c563c7f93e64700e7460f6fb8755886dd4b606b8510a026b
arm64v1win=7db68a1f165b4bdab997abf7d91e00230c9112b4e75419a7564fa1dc44780716
arm64v1mingw=7db68a1f165b4bdab997abf7d91e00230c9112b4e75419a7564fa1dc44780716
arm64v1linux=4d31ddab46a215c9c563c7f93e64700e7460f6fb8755886dd4b606b8510a026b
arm64v1musl=4d31ddab46a215c9c563c7f93e64700e7460f6fb8755886dd4b606b8510a026b
arm64v1glibc=4d31ddab46a215c9c563c7f93e64700e7460f6fb8755886dd4b606b8510a026b
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
