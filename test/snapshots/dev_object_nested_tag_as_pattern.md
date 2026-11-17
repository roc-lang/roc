# META
~~~ini
description=Nested tag matching through as-pattern wrapper
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

Error : [Exit(I64), NotFound]
Result : [Ok(I64), Err(Error)]

extract_code : Result -> I64
extract_code = |result|
    match result {
        Ok(n) => n
        Err(Exit(code) as inner) =>
            match inner {
                Exit(_) => code
                _ => -2
            }
        Err(_) => -1
    }

main = Str.inspect(extract_code(Err(Exit(42))))
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : Str }
    exposes []
    packages {}
    provides { "roc_main": main_for_host }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
    }

main_for_host : Str
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# app
extract_code = |result| match result {
	Ok(n) => n
	Err(Exit(code) as inner) => match Exit(code) as inner {
		Exit(_) => code
		_ => -2
	}
	Err(_) => -1
}
main = inspect(extract_code(Err(Exit(42))))

~~~
# DEV OUTPUT
~~~ini
x64mac=e6530fe2786c3a33f51226f9f467465c93c44b725a7c576be9204196104c2c6b
x64win=cbdf289494dd9a293436c1e759f365c72d8a48df2d1aa4d7ae5e9c3dcdad0325
x64mingw=cbdf289494dd9a293436c1e759f365c72d8a48df2d1aa4d7ae5e9c3dcdad0325
x64freebsd=3c7c4b830278c57a68aa9fe84aa6a56028b9c4fbda1ae2283161877378618cdb
x64openbsd=1ba0f5cad5b7fe7bda519c003c48ea2f3115c555f0f84eccdbb1c78a874f9a4b
x64netbsd=dca7edd1a2e76666c397eac8f01523b7de4408a16f73873236d0a1bf669a0c7b
x64musl=dca7edd1a2e76666c397eac8f01523b7de4408a16f73873236d0a1bf669a0c7b
x64glibc=dca7edd1a2e76666c397eac8f01523b7de4408a16f73873236d0a1bf669a0c7b
x64linux=dca7edd1a2e76666c397eac8f01523b7de4408a16f73873236d0a1bf669a0c7b
x64elf=dca7edd1a2e76666c397eac8f01523b7de4408a16f73873236d0a1bf669a0c7b
x64v1mac=e6530fe2786c3a33f51226f9f467465c93c44b725a7c576be9204196104c2c6b
x64v1win=cbdf289494dd9a293436c1e759f365c72d8a48df2d1aa4d7ae5e9c3dcdad0325
x64v1mingw=cbdf289494dd9a293436c1e759f365c72d8a48df2d1aa4d7ae5e9c3dcdad0325
x64v1freebsd=3c7c4b830278c57a68aa9fe84aa6a56028b9c4fbda1ae2283161877378618cdb
x64v1openbsd=1ba0f5cad5b7fe7bda519c003c48ea2f3115c555f0f84eccdbb1c78a874f9a4b
x64v1netbsd=dca7edd1a2e76666c397eac8f01523b7de4408a16f73873236d0a1bf669a0c7b
x64v1musl=dca7edd1a2e76666c397eac8f01523b7de4408a16f73873236d0a1bf669a0c7b
x64v1glibc=dca7edd1a2e76666c397eac8f01523b7de4408a16f73873236d0a1bf669a0c7b
x64v1linux=dca7edd1a2e76666c397eac8f01523b7de4408a16f73873236d0a1bf669a0c7b
x64v1elf=dca7edd1a2e76666c397eac8f01523b7de4408a16f73873236d0a1bf669a0c7b
arm64mac=33965f80fb2bbe2675cc088bc3fc7f7cb7287b783c6d584308ee7c73f476c74d
arm64win=836ab6c13655d5f09148c9688b82cc63ba2f18b0e1a7961561aa15caeb4f7cd3
arm64mingw=836ab6c13655d5f09148c9688b82cc63ba2f18b0e1a7961561aa15caeb4f7cd3
arm64linux=b154e1ed1d57baa9a91080450f3e8320f8e567f559949cf618a87e90e2244be0
arm64musl=b154e1ed1d57baa9a91080450f3e8320f8e567f559949cf618a87e90e2244be0
arm64glibc=b154e1ed1d57baa9a91080450f3e8320f8e567f559949cf618a87e90e2244be0
arm64v1win=836ab6c13655d5f09148c9688b82cc63ba2f18b0e1a7961561aa15caeb4f7cd3
arm64v1mingw=836ab6c13655d5f09148c9688b82cc63ba2f18b0e1a7961561aa15caeb4f7cd3
arm64v1linux=b154e1ed1d57baa9a91080450f3e8320f8e567f559949cf618a87e90e2244be0
arm64v1musl=b154e1ed1d57baa9a91080450f3e8320f8e567f559949cf618a87e90e2244be0
arm64v1glibc=b154e1ed1d57baa9a91080450f3e8320f8e567f559949cf618a87e90e2244be0
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
