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
x64mac=63314848c5ddb61a3e6003a95f67f58aee3b50272382564db5497cbfecc2451b
x64win=b2074400df305bb420282fbbdc2fdbfec4dd5407dd715a6fed36551a71e65a71
x64mingw=b2074400df305bb420282fbbdc2fdbfec4dd5407dd715a6fed36551a71e65a71
x64freebsd=e439e642789c849d02dfd5a6b0c3d8ec6e62ac96f83f29096d8a78ad36c477a9
x64openbsd=2928183fd6e18ffb09d430fc4bfcdbd5c0dbcc8b5646c80b18a8647433c72f51
x64netbsd=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64musl=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64glibc=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64linux=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64elf=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64v1mac=63314848c5ddb61a3e6003a95f67f58aee3b50272382564db5497cbfecc2451b
x64v1win=b2074400df305bb420282fbbdc2fdbfec4dd5407dd715a6fed36551a71e65a71
x64v1mingw=b2074400df305bb420282fbbdc2fdbfec4dd5407dd715a6fed36551a71e65a71
x64v1freebsd=e439e642789c849d02dfd5a6b0c3d8ec6e62ac96f83f29096d8a78ad36c477a9
x64v1openbsd=2928183fd6e18ffb09d430fc4bfcdbd5c0dbcc8b5646c80b18a8647433c72f51
x64v1netbsd=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64v1musl=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64v1glibc=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64v1linux=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64v1elf=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
arm64mac=252b163250d1af3880e7bd5463858f1c962b0adcf9f0380e280ef2f6f6a6e260
arm64win=22b39ff0ed33f36b14234b308c68931cd6cc73fafe3dac9f0c1f9680e6d8fb10
arm64mingw=22b39ff0ed33f36b14234b308c68931cd6cc73fafe3dac9f0c1f9680e6d8fb10
arm64linux=dd328f20b7eed6dc109d695a927b2aecdc59250e43b931b4a71ff5b8a1c69682
arm64musl=dd328f20b7eed6dc109d695a927b2aecdc59250e43b931b4a71ff5b8a1c69682
arm64glibc=dd328f20b7eed6dc109d695a927b2aecdc59250e43b931b4a71ff5b8a1c69682
arm64v1win=22b39ff0ed33f36b14234b308c68931cd6cc73fafe3dac9f0c1f9680e6d8fb10
arm64v1mingw=22b39ff0ed33f36b14234b308c68931cd6cc73fafe3dac9f0c1f9680e6d8fb10
arm64v1linux=dd328f20b7eed6dc109d695a927b2aecdc59250e43b931b4a71ff5b8a1c69682
arm64v1musl=dd328f20b7eed6dc109d695a927b2aecdc59250e43b931b4a71ff5b8a1c69682
arm64v1glibc=dd328f20b7eed6dc109d695a927b2aecdc59250e43b931b4a71ff5b8a1c69682
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
