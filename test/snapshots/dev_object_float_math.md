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
x64mac=82d64fb118f3a3ca13b7e8ab232fcb3d6eaf0de1a725f3d11445e20030b59ee3
x64win=07ada194ced28237c6495ab60e995924295789c486b151d23e47d4ba517c3035
x64mingw=07ada194ced28237c6495ab60e995924295789c486b151d23e47d4ba517c3035
x64freebsd=75ed4c7b6323afa32fe104e167e8f0443ee03a1071f64ed1633625bbcb4a0310
x64openbsd=3a5f1fea8fde17ac3d28b0dbcfd6ce145019e6f166faad83393f196766d075a7
x64netbsd=b4e2dece3ca792bd1c7ea95b62a0761fd261d2342bd450d6aaac6d9b7f1c44ca
x64musl=b4e2dece3ca792bd1c7ea95b62a0761fd261d2342bd450d6aaac6d9b7f1c44ca
x64glibc=b4e2dece3ca792bd1c7ea95b62a0761fd261d2342bd450d6aaac6d9b7f1c44ca
x64linux=b4e2dece3ca792bd1c7ea95b62a0761fd261d2342bd450d6aaac6d9b7f1c44ca
x64elf=b4e2dece3ca792bd1c7ea95b62a0761fd261d2342bd450d6aaac6d9b7f1c44ca
x64v1mac=82d64fb118f3a3ca13b7e8ab232fcb3d6eaf0de1a725f3d11445e20030b59ee3
x64v1win=07ada194ced28237c6495ab60e995924295789c486b151d23e47d4ba517c3035
x64v1mingw=07ada194ced28237c6495ab60e995924295789c486b151d23e47d4ba517c3035
x64v1freebsd=75ed4c7b6323afa32fe104e167e8f0443ee03a1071f64ed1633625bbcb4a0310
x64v1openbsd=3a5f1fea8fde17ac3d28b0dbcfd6ce145019e6f166faad83393f196766d075a7
x64v1netbsd=b4e2dece3ca792bd1c7ea95b62a0761fd261d2342bd450d6aaac6d9b7f1c44ca
x64v1musl=b4e2dece3ca792bd1c7ea95b62a0761fd261d2342bd450d6aaac6d9b7f1c44ca
x64v1glibc=b4e2dece3ca792bd1c7ea95b62a0761fd261d2342bd450d6aaac6d9b7f1c44ca
x64v1linux=b4e2dece3ca792bd1c7ea95b62a0761fd261d2342bd450d6aaac6d9b7f1c44ca
x64v1elf=b4e2dece3ca792bd1c7ea95b62a0761fd261d2342bd450d6aaac6d9b7f1c44ca
arm64mac=85b0eb4b323af39fd3c48cf6854d3b1b8ea65ea14bfc21583e21b817981e8a91
arm64win=231d21e5b31e5fbe484d07ae85983ea8d56889c2e8f5036ee7ac7def0a5453e7
arm64mingw=231d21e5b31e5fbe484d07ae85983ea8d56889c2e8f5036ee7ac7def0a5453e7
arm64linux=ce8a063be314cca6f3810ca9ac283b8f53762172c5f8f18ea7d2012faf0ca68e
arm64musl=ce8a063be314cca6f3810ca9ac283b8f53762172c5f8f18ea7d2012faf0ca68e
arm64glibc=ce8a063be314cca6f3810ca9ac283b8f53762172c5f8f18ea7d2012faf0ca68e
arm64v1win=231d21e5b31e5fbe484d07ae85983ea8d56889c2e8f5036ee7ac7def0a5453e7
arm64v1mingw=231d21e5b31e5fbe484d07ae85983ea8d56889c2e8f5036ee7ac7def0a5453e7
arm64v1linux=ce8a063be314cca6f3810ca9ac283b8f53762172c5f8f18ea7d2012faf0ca68e
arm64v1musl=ce8a063be314cca6f3810ca9ac283b8f53762172c5f8f18ea7d2012faf0ca68e
arm64v1glibc=ce8a063be314cca6f3810ca9ac283b8f53762172c5f8f18ea7d2012faf0ca68e
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
