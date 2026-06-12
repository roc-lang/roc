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
        inputs: "targets/",
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
x64mac=6bfa91727900a635a1cb3809b5570006214dad2beff8e2c5413b7b835dae8480
x64win=0fb686ad250c7a686f8a8210b6e6765546ab8014a71a567a14cba78a53bc24a3
x64freebsd=fff42fe15ee8e93a1831b9ede0925e05b26d848c23d4b365f8bbfcbf8052013f
x64openbsd=fff42fe15ee8e93a1831b9ede0925e05b26d848c23d4b365f8bbfcbf8052013f
x64netbsd=fff42fe15ee8e93a1831b9ede0925e05b26d848c23d4b365f8bbfcbf8052013f
x64musl=fff42fe15ee8e93a1831b9ede0925e05b26d848c23d4b365f8bbfcbf8052013f
x64glibc=fff42fe15ee8e93a1831b9ede0925e05b26d848c23d4b365f8bbfcbf8052013f
x64linux=fff42fe15ee8e93a1831b9ede0925e05b26d848c23d4b365f8bbfcbf8052013f
x64elf=fff42fe15ee8e93a1831b9ede0925e05b26d848c23d4b365f8bbfcbf8052013f
arm64mac=8fccddb20d384d11c49f42248597d8000efc73db1002b4b3c81947fa0eb88087
arm64win=57733007b9883218cd647d37090f33331ce8379ed21d62e858c7ac6601f4e106
arm64linux=e06db3252edb13cec27b425a53b12b57e8427a3ba30d7cd03a98f8271f5058f5
arm64musl=e06db3252edb13cec27b425a53b12b57e8427a3ba30d7cd03a98f8271f5058f5
arm64glibc=e06db3252edb13cec27b425a53b12b57e8427a3ba30d7cd03a98f8271f5058f5
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
