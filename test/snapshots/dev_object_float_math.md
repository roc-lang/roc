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
x64mac=d6a22e8862c58496923762d1c23aab7a6f2b2d6f5f6f00db878aeda52b22a868
x64win=bdb27236dd5d8247e3796b5416cda963ca53d3223e254fa71ee6aeb3661cf964
x64mingw=bdb27236dd5d8247e3796b5416cda963ca53d3223e254fa71ee6aeb3661cf964
x64freebsd=8802e192359f9fd700f21a2b07f844f6d95e20e2442c81d3e5c45179fa27a046
x64openbsd=63df0c0a134e11ee40af5e41a79cd59daf3185bcb579712cb54804c11ccf65de
x64netbsd=86ce9829b609f3463700c28c3e7ceac60b94937d54412978ca356b256eaca469
x64musl=86ce9829b609f3463700c28c3e7ceac60b94937d54412978ca356b256eaca469
x64glibc=86ce9829b609f3463700c28c3e7ceac60b94937d54412978ca356b256eaca469
x64linux=86ce9829b609f3463700c28c3e7ceac60b94937d54412978ca356b256eaca469
x64elf=86ce9829b609f3463700c28c3e7ceac60b94937d54412978ca356b256eaca469
x64v1mac=d6a22e8862c58496923762d1c23aab7a6f2b2d6f5f6f00db878aeda52b22a868
x64v1win=bdb27236dd5d8247e3796b5416cda963ca53d3223e254fa71ee6aeb3661cf964
x64v1mingw=bdb27236dd5d8247e3796b5416cda963ca53d3223e254fa71ee6aeb3661cf964
x64v1freebsd=8802e192359f9fd700f21a2b07f844f6d95e20e2442c81d3e5c45179fa27a046
x64v1openbsd=63df0c0a134e11ee40af5e41a79cd59daf3185bcb579712cb54804c11ccf65de
x64v1netbsd=86ce9829b609f3463700c28c3e7ceac60b94937d54412978ca356b256eaca469
x64v1musl=86ce9829b609f3463700c28c3e7ceac60b94937d54412978ca356b256eaca469
x64v1glibc=86ce9829b609f3463700c28c3e7ceac60b94937d54412978ca356b256eaca469
x64v1linux=86ce9829b609f3463700c28c3e7ceac60b94937d54412978ca356b256eaca469
x64v1elf=86ce9829b609f3463700c28c3e7ceac60b94937d54412978ca356b256eaca469
arm64mac=768c83dedccbcdcbdb39eff33163cc6d7453cfcad3fa6b45b5d652d133fe6028
arm64win=3089d96c20c21887ac9309a645d3459c7e1e7112ccff834580f27abf710881ab
arm64mingw=3089d96c20c21887ac9309a645d3459c7e1e7112ccff834580f27abf710881ab
arm64linux=4c26b0e5da0566c6646e82504277898fa5b840b3b99c36b1cf794b8ff0b86445
arm64musl=4c26b0e5da0566c6646e82504277898fa5b840b3b99c36b1cf794b8ff0b86445
arm64glibc=4c26b0e5da0566c6646e82504277898fa5b840b3b99c36b1cf794b8ff0b86445
arm64v1win=3089d96c20c21887ac9309a645d3459c7e1e7112ccff834580f27abf710881ab
arm64v1mingw=3089d96c20c21887ac9309a645d3459c7e1e7112ccff834580f27abf710881ab
arm64v1linux=4c26b0e5da0566c6646e82504277898fa5b840b3b99c36b1cf794b8ff0b86445
arm64v1musl=4c26b0e5da0566c6646e82504277898fa5b840b3b99c36b1cf794b8ff0b86445
arm64v1glibc=4c26b0e5da0566c6646e82504277898fa5b840b3b99c36b1cf794b8ff0b86445
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
