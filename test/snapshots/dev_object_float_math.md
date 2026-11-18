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
x64mac=35d83dc4b972e8a4bda2643c6805d08ea0e53a081d6882ce3707a1b234bb54c7
x64win=97f58cd7eb7df97975abb3e1e0d5397f4566852433c1aaaa1d0daf6a979e353e
x64mingw=97f58cd7eb7df97975abb3e1e0d5397f4566852433c1aaaa1d0daf6a979e353e
x64freebsd=b72055676e4d60102c6b7c5cf94248a32350ecf30e496d7c2a0e9941d010278d
x64openbsd=9ee4ac769f4065dbfb5f50ebdf811abb276bd0f518ae9faf1aeb56e5f73590b1
x64netbsd=accb3ead34aece2b5440359c9b489ccb1904083c2b9fd319c411c06d580175ae
x64musl=accb3ead34aece2b5440359c9b489ccb1904083c2b9fd319c411c06d580175ae
x64glibc=accb3ead34aece2b5440359c9b489ccb1904083c2b9fd319c411c06d580175ae
x64linux=accb3ead34aece2b5440359c9b489ccb1904083c2b9fd319c411c06d580175ae
x64elf=accb3ead34aece2b5440359c9b489ccb1904083c2b9fd319c411c06d580175ae
x64v1mac=35d83dc4b972e8a4bda2643c6805d08ea0e53a081d6882ce3707a1b234bb54c7
x64v1win=97f58cd7eb7df97975abb3e1e0d5397f4566852433c1aaaa1d0daf6a979e353e
x64v1mingw=97f58cd7eb7df97975abb3e1e0d5397f4566852433c1aaaa1d0daf6a979e353e
x64v1freebsd=b72055676e4d60102c6b7c5cf94248a32350ecf30e496d7c2a0e9941d010278d
x64v1openbsd=9ee4ac769f4065dbfb5f50ebdf811abb276bd0f518ae9faf1aeb56e5f73590b1
x64v1netbsd=accb3ead34aece2b5440359c9b489ccb1904083c2b9fd319c411c06d580175ae
x64v1musl=accb3ead34aece2b5440359c9b489ccb1904083c2b9fd319c411c06d580175ae
x64v1glibc=accb3ead34aece2b5440359c9b489ccb1904083c2b9fd319c411c06d580175ae
x64v1linux=accb3ead34aece2b5440359c9b489ccb1904083c2b9fd319c411c06d580175ae
x64v1elf=accb3ead34aece2b5440359c9b489ccb1904083c2b9fd319c411c06d580175ae
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
