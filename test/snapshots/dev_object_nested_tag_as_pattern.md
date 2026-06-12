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
x64mac=8010c5534b1921421d0ae08898b55b40a9542a2742f8862728e484596f04fb42
x64win=0fb686ad250c7a686f8a8210b6e6765546ab8014a71a567a14cba78a53bc24a3
x64freebsd=f3e710f1d5277e12b4c5c75bc245f1346cd0d42cedfe2a5a646ed490bbdae070
x64openbsd=f3e710f1d5277e12b4c5c75bc245f1346cd0d42cedfe2a5a646ed490bbdae070
x64netbsd=f3e710f1d5277e12b4c5c75bc245f1346cd0d42cedfe2a5a646ed490bbdae070
x64musl=f3e710f1d5277e12b4c5c75bc245f1346cd0d42cedfe2a5a646ed490bbdae070
x64glibc=f3e710f1d5277e12b4c5c75bc245f1346cd0d42cedfe2a5a646ed490bbdae070
x64linux=f3e710f1d5277e12b4c5c75bc245f1346cd0d42cedfe2a5a646ed490bbdae070
x64elf=f3e710f1d5277e12b4c5c75bc245f1346cd0d42cedfe2a5a646ed490bbdae070
arm64mac=72fa5440705af64baffdd4917facd1c1d00dba2908fc91c5ddd0ea3ed117ae34
arm64win=57733007b9883218cd647d37090f33331ce8379ed21d62e858c7ac6601f4e106
arm64linux=2a7d36260ecff5d4fd05bcf6d700fabd1e3d2c68a5f4b7542aadfc4bc2ac5cfe
arm64musl=2a7d36260ecff5d4fd05bcf6d700fabd1e3d2c68a5f4b7542aadfc4bc2ac5cfe
arm64glibc=2a7d36260ecff5d4fd05bcf6d700fabd1e3d2c68a5f4b7542aadfc4bc2ac5cfe
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
