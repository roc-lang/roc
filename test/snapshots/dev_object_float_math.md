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
x64mac=02eae88a695a27b61e07604166763c21900e95ceff2d7dd33c9ec3bcdfb294cf
x64win=51f658806051e80a40d4488029ae4e9efa01fd7a74b3cd202d2a7bb94556672a
x64mingw=51f658806051e80a40d4488029ae4e9efa01fd7a74b3cd202d2a7bb94556672a
x64freebsd=e34ffea38756244efebfc841d2f1bd96c09cdb0887b3087d44b1ee1d589deb81
x64openbsd=69a6f9a1e6edf0c077b5be963de20e45624bfc2b6290fb473da9942915062dc7
x64netbsd=fcab3b43f3af2578fa6ddcf9ed6aa337f7156e140ed68e95d9f609792d553c73
x64musl=fcab3b43f3af2578fa6ddcf9ed6aa337f7156e140ed68e95d9f609792d553c73
x64glibc=fcab3b43f3af2578fa6ddcf9ed6aa337f7156e140ed68e95d9f609792d553c73
x64linux=fcab3b43f3af2578fa6ddcf9ed6aa337f7156e140ed68e95d9f609792d553c73
x64elf=fcab3b43f3af2578fa6ddcf9ed6aa337f7156e140ed68e95d9f609792d553c73
x64v1mac=02eae88a695a27b61e07604166763c21900e95ceff2d7dd33c9ec3bcdfb294cf
x64v1win=51f658806051e80a40d4488029ae4e9efa01fd7a74b3cd202d2a7bb94556672a
x64v1mingw=51f658806051e80a40d4488029ae4e9efa01fd7a74b3cd202d2a7bb94556672a
x64v1freebsd=e34ffea38756244efebfc841d2f1bd96c09cdb0887b3087d44b1ee1d589deb81
x64v1openbsd=69a6f9a1e6edf0c077b5be963de20e45624bfc2b6290fb473da9942915062dc7
x64v1netbsd=fcab3b43f3af2578fa6ddcf9ed6aa337f7156e140ed68e95d9f609792d553c73
x64v1musl=fcab3b43f3af2578fa6ddcf9ed6aa337f7156e140ed68e95d9f609792d553c73
x64v1glibc=fcab3b43f3af2578fa6ddcf9ed6aa337f7156e140ed68e95d9f609792d553c73
x64v1linux=fcab3b43f3af2578fa6ddcf9ed6aa337f7156e140ed68e95d9f609792d553c73
x64v1elf=fcab3b43f3af2578fa6ddcf9ed6aa337f7156e140ed68e95d9f609792d553c73
arm64mac=a8923812dccd565f488b9f99248865f65e1a9c1325c359ab52a7158fa23b50dc
arm64win=eae74510c79496e4698a0ebaf9e5f12a07a5eba1a4a11765b21e52f9733abbd3
arm64mingw=eae74510c79496e4698a0ebaf9e5f12a07a5eba1a4a11765b21e52f9733abbd3
arm64linux=6d572108e66132b68b8ac6ff1def3a1c3166ad3604f9f39898254bed4d6d2017
arm64musl=6d572108e66132b68b8ac6ff1def3a1c3166ad3604f9f39898254bed4d6d2017
arm64glibc=6d572108e66132b68b8ac6ff1def3a1c3166ad3604f9f39898254bed4d6d2017
arm64v1win=eae74510c79496e4698a0ebaf9e5f12a07a5eba1a4a11765b21e52f9733abbd3
arm64v1mingw=eae74510c79496e4698a0ebaf9e5f12a07a5eba1a4a11765b21e52f9733abbd3
arm64v1linux=6d572108e66132b68b8ac6ff1def3a1c3166ad3604f9f39898254bed4d6d2017
arm64v1musl=6d572108e66132b68b8ac6ff1def3a1c3166ad3604f9f39898254bed4d6d2017
arm64v1glibc=6d572108e66132b68b8ac6ff1def3a1c3166ad3604f9f39898254bed4d6d2017
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
