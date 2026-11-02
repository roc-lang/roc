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
x64mac=fe3c0e793a38f23178ff638e49c77df1ed533fa6384cf79f88aee10428ccd695
x64win=c548d9ce37046a86f5e83ce16687721147f9ec5008d4ef37c131719f37288398
x64mingw=c548d9ce37046a86f5e83ce16687721147f9ec5008d4ef37c131719f37288398
x64freebsd=d623ccb33a1c37ea1f5eba3c77e9c5c86b7552eb6cf0d722e537af34da9285d8
x64openbsd=827ba0450ad5804b2b2ce68950172327c7329d00646c9cb1aa2967608fbdbfa2
x64netbsd=55b6c5d8750324fcdcb1fff56dc818eb4cd238db579346b7390249873b40f150
x64musl=55b6c5d8750324fcdcb1fff56dc818eb4cd238db579346b7390249873b40f150
x64glibc=55b6c5d8750324fcdcb1fff56dc818eb4cd238db579346b7390249873b40f150
x64linux=55b6c5d8750324fcdcb1fff56dc818eb4cd238db579346b7390249873b40f150
x64elf=55b6c5d8750324fcdcb1fff56dc818eb4cd238db579346b7390249873b40f150
x64v1mac=fe3c0e793a38f23178ff638e49c77df1ed533fa6384cf79f88aee10428ccd695
x64v1win=c548d9ce37046a86f5e83ce16687721147f9ec5008d4ef37c131719f37288398
x64v1mingw=c548d9ce37046a86f5e83ce16687721147f9ec5008d4ef37c131719f37288398
x64v1freebsd=d623ccb33a1c37ea1f5eba3c77e9c5c86b7552eb6cf0d722e537af34da9285d8
x64v1openbsd=827ba0450ad5804b2b2ce68950172327c7329d00646c9cb1aa2967608fbdbfa2
x64v1netbsd=55b6c5d8750324fcdcb1fff56dc818eb4cd238db579346b7390249873b40f150
x64v1musl=55b6c5d8750324fcdcb1fff56dc818eb4cd238db579346b7390249873b40f150
x64v1glibc=55b6c5d8750324fcdcb1fff56dc818eb4cd238db579346b7390249873b40f150
x64v1linux=55b6c5d8750324fcdcb1fff56dc818eb4cd238db579346b7390249873b40f150
x64v1elf=55b6c5d8750324fcdcb1fff56dc818eb4cd238db579346b7390249873b40f150
arm64mac=7b70e1c90f38e50d240627cae790114252fa666185104ebd34338d24ba67339b
arm64win=a08c5fb5b04e532624cf7153847a368e6909176feb940e32a055333059cb2148
arm64mingw=a08c5fb5b04e532624cf7153847a368e6909176feb940e32a055333059cb2148
arm64linux=4a3c5b71c475462c79438d5d0d85eafcc7b84a2dea8c7392dca86b2b6514dba5
arm64musl=4a3c5b71c475462c79438d5d0d85eafcc7b84a2dea8c7392dca86b2b6514dba5
arm64glibc=4a3c5b71c475462c79438d5d0d85eafcc7b84a2dea8c7392dca86b2b6514dba5
arm64v1win=a08c5fb5b04e532624cf7153847a368e6909176feb940e32a055333059cb2148
arm64v1mingw=a08c5fb5b04e532624cf7153847a368e6909176feb940e32a055333059cb2148
arm64v1linux=4a3c5b71c475462c79438d5d0d85eafcc7b84a2dea8c7392dca86b2b6514dba5
arm64v1musl=4a3c5b71c475462c79438d5d0d85eafcc7b84a2dea8c7392dca86b2b6514dba5
arm64v1glibc=4a3c5b71c475462c79438d5d0d85eafcc7b84a2dea8c7392dca86b2b6514dba5
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
