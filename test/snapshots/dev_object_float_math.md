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
x64mac=30d509923c8935abe99bfa7f8cbb961bf14cd38e063634e39569a1f7ee40ef93
x64win=cc7045ab1e841109a32a6929666c852d718fd0ae7f156eb31c1402f7174f6b93
x64mingw=cc7045ab1e841109a32a6929666c852d718fd0ae7f156eb31c1402f7174f6b93
x64freebsd=e937be02f307c196c6b13a8cf255a4b00e91a65bb1a7cb36549c52eafbeeb656
x64openbsd=8c538df547e0df391231b35c2bab6adb91d5d60d9a1386dbc03fa5931bfd9160
x64netbsd=d645133ac1982d3ef7764d6a267836dd48a6a281f08f966664f4a122efb20fa8
x64musl=d645133ac1982d3ef7764d6a267836dd48a6a281f08f966664f4a122efb20fa8
x64glibc=d645133ac1982d3ef7764d6a267836dd48a6a281f08f966664f4a122efb20fa8
x64linux=d645133ac1982d3ef7764d6a267836dd48a6a281f08f966664f4a122efb20fa8
x64elf=d645133ac1982d3ef7764d6a267836dd48a6a281f08f966664f4a122efb20fa8
x64v1mac=30d509923c8935abe99bfa7f8cbb961bf14cd38e063634e39569a1f7ee40ef93
x64v1win=cc7045ab1e841109a32a6929666c852d718fd0ae7f156eb31c1402f7174f6b93
x64v1mingw=cc7045ab1e841109a32a6929666c852d718fd0ae7f156eb31c1402f7174f6b93
x64v1freebsd=e937be02f307c196c6b13a8cf255a4b00e91a65bb1a7cb36549c52eafbeeb656
x64v1openbsd=8c538df547e0df391231b35c2bab6adb91d5d60d9a1386dbc03fa5931bfd9160
x64v1netbsd=d645133ac1982d3ef7764d6a267836dd48a6a281f08f966664f4a122efb20fa8
x64v1musl=d645133ac1982d3ef7764d6a267836dd48a6a281f08f966664f4a122efb20fa8
x64v1glibc=d645133ac1982d3ef7764d6a267836dd48a6a281f08f966664f4a122efb20fa8
x64v1linux=d645133ac1982d3ef7764d6a267836dd48a6a281f08f966664f4a122efb20fa8
x64v1elf=d645133ac1982d3ef7764d6a267836dd48a6a281f08f966664f4a122efb20fa8
arm64mac=bacaffbf4abbaebc17cfaf4c0bc5fd983f4acaaa536a882bada07a3eefe3c485
arm64win=17bc570b8b2249636da4422d21536840cc1415fc28a17a5f5cabdc3985d88fb8
arm64mingw=17bc570b8b2249636da4422d21536840cc1415fc28a17a5f5cabdc3985d88fb8
arm64linux=68a8be195c0b399e604236d3ac5cde4edcf34e1a91f8345e56c719450c701c2b
arm64musl=68a8be195c0b399e604236d3ac5cde4edcf34e1a91f8345e56c719450c701c2b
arm64glibc=68a8be195c0b399e604236d3ac5cde4edcf34e1a91f8345e56c719450c701c2b
arm64v1win=17bc570b8b2249636da4422d21536840cc1415fc28a17a5f5cabdc3985d88fb8
arm64v1mingw=17bc570b8b2249636da4422d21536840cc1415fc28a17a5f5cabdc3985d88fb8
arm64v1linux=68a8be195c0b399e604236d3ac5cde4edcf34e1a91f8345e56c719450c701c2b
arm64v1musl=68a8be195c0b399e604236d3ac5cde4edcf34e1a91f8345e56c719450c701c2b
arm64v1glibc=68a8be195c0b399e604236d3ac5cde4edcf34e1a91f8345e56c719450c701c2b
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
