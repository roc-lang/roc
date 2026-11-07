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
x64mac=b98d1c4ba235d2410cda436bbb5f29711516acf6fbbf7ee813c67bb82934512d
x64win=753e8684e488ee6b2a175fa35f2ffd55eb20671da0a68ee8790f7e5345d4b192
x64mingw=753e8684e488ee6b2a175fa35f2ffd55eb20671da0a68ee8790f7e5345d4b192
x64freebsd=6c8ef6ce4f64acd8e5fdabac20583ae22514e1bb20aa0906851a0dac6794644c
x64openbsd=27f3516943d39a69eacaf4ddfda47f458e240ffa91a500f39321990028c4b52a
x64netbsd=8036405b44f954cb041d80623b7cc763b5d1df81f0dfe06de89f75c489fa4635
x64musl=8036405b44f954cb041d80623b7cc763b5d1df81f0dfe06de89f75c489fa4635
x64glibc=8036405b44f954cb041d80623b7cc763b5d1df81f0dfe06de89f75c489fa4635
x64linux=8036405b44f954cb041d80623b7cc763b5d1df81f0dfe06de89f75c489fa4635
x64elf=8036405b44f954cb041d80623b7cc763b5d1df81f0dfe06de89f75c489fa4635
x64v1mac=b98d1c4ba235d2410cda436bbb5f29711516acf6fbbf7ee813c67bb82934512d
x64v1win=753e8684e488ee6b2a175fa35f2ffd55eb20671da0a68ee8790f7e5345d4b192
x64v1mingw=753e8684e488ee6b2a175fa35f2ffd55eb20671da0a68ee8790f7e5345d4b192
x64v1freebsd=6c8ef6ce4f64acd8e5fdabac20583ae22514e1bb20aa0906851a0dac6794644c
x64v1openbsd=27f3516943d39a69eacaf4ddfda47f458e240ffa91a500f39321990028c4b52a
x64v1netbsd=8036405b44f954cb041d80623b7cc763b5d1df81f0dfe06de89f75c489fa4635
x64v1musl=8036405b44f954cb041d80623b7cc763b5d1df81f0dfe06de89f75c489fa4635
x64v1glibc=8036405b44f954cb041d80623b7cc763b5d1df81f0dfe06de89f75c489fa4635
x64v1linux=8036405b44f954cb041d80623b7cc763b5d1df81f0dfe06de89f75c489fa4635
x64v1elf=8036405b44f954cb041d80623b7cc763b5d1df81f0dfe06de89f75c489fa4635
arm64mac=6a940f307f777972ccce88d42d2e205d8b0a8b33261113750da666fb980e2c72
arm64win=3a0746d90ff335aca468f63af4efbe3357069ce127d422dd14b688e8141e3683
arm64mingw=3a0746d90ff335aca468f63af4efbe3357069ce127d422dd14b688e8141e3683
arm64linux=8e00adb625be397fd1f57cc98946c7e83bc50476e478d85e7d36846475dc4093
arm64musl=8e00adb625be397fd1f57cc98946c7e83bc50476e478d85e7d36846475dc4093
arm64glibc=8e00adb625be397fd1f57cc98946c7e83bc50476e478d85e7d36846475dc4093
arm64v1win=3a0746d90ff335aca468f63af4efbe3357069ce127d422dd14b688e8141e3683
arm64v1mingw=3a0746d90ff335aca468f63af4efbe3357069ce127d422dd14b688e8141e3683
arm64v1linux=8e00adb625be397fd1f57cc98946c7e83bc50476e478d85e7d36846475dc4093
arm64v1musl=8e00adb625be397fd1f57cc98946c7e83bc50476e478d85e7d36846475dc4093
arm64v1glibc=8e00adb625be397fd1f57cc98946c7e83bc50476e478d85e7d36846475dc4093
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
