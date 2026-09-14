# META
~~~ini
description=Nested tag pattern matching with Err(Exit(code))
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
        Err(Exit(code)) => code
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
	Err(Exit(code)) => code
	Err(_) => -1
}
main = inspect(extract_code(Err(Exit(42))))

~~~
# DEV OUTPUT
~~~ini
x64mac=40ac3412e35ec977f27841325006f5b51d05a466d59d3432fbb2234f815d581c
x64win=428f5e6e9dbb434cc8df9dd5dc959938d27f1f2e4eff9efda840316222a341f8
x64mingw=428f5e6e9dbb434cc8df9dd5dc959938d27f1f2e4eff9efda840316222a341f8
x64freebsd=e439e642789c849d02dfd5a6b0c3d8ec6e62ac96f83f29096d8a78ad36c477a9
x64openbsd=2928183fd6e18ffb09d430fc4bfcdbd5c0dbcc8b5646c80b18a8647433c72f51
x64netbsd=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64musl=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64glibc=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64linux=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64elf=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64v1mac=40ac3412e35ec977f27841325006f5b51d05a466d59d3432fbb2234f815d581c
x64v1win=428f5e6e9dbb434cc8df9dd5dc959938d27f1f2e4eff9efda840316222a341f8
x64v1mingw=428f5e6e9dbb434cc8df9dd5dc959938d27f1f2e4eff9efda840316222a341f8
x64v1freebsd=e439e642789c849d02dfd5a6b0c3d8ec6e62ac96f83f29096d8a78ad36c477a9
x64v1openbsd=2928183fd6e18ffb09d430fc4bfcdbd5c0dbcc8b5646c80b18a8647433c72f51
x64v1netbsd=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64v1musl=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64v1glibc=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64v1linux=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
x64v1elf=c1170cc6ba41e26b31cbdb0585d0259b13be9e8faac647e6a49d7a44ab224f66
arm64mac=601a6433b78af2f678b7b23c7e58e62e7126ead88d4866847a2d2f3440eb4ea2
arm64win=a8e70c607a1fdaee97cbd0b9f27774f6087087ff3bf1b950f9f416b6ccc2fcc3
arm64mingw=a8e70c607a1fdaee97cbd0b9f27774f6087087ff3bf1b950f9f416b6ccc2fcc3
arm64linux=dd328f20b7eed6dc109d695a927b2aecdc59250e43b931b4a71ff5b8a1c69682
arm64musl=dd328f20b7eed6dc109d695a927b2aecdc59250e43b931b4a71ff5b8a1c69682
arm64glibc=dd328f20b7eed6dc109d695a927b2aecdc59250e43b931b4a71ff5b8a1c69682
arm64v1win=a8e70c607a1fdaee97cbd0b9f27774f6087087ff3bf1b950f9f416b6ccc2fcc3
arm64v1mingw=a8e70c607a1fdaee97cbd0b9f27774f6087087ff3bf1b950f9f416b6ccc2fcc3
arm64v1linux=dd328f20b7eed6dc109d695a927b2aecdc59250e43b931b4a71ff5b8a1c69682
arm64v1musl=dd328f20b7eed6dc109d695a927b2aecdc59250e43b931b4a71ff5b8a1c69682
arm64v1glibc=dd328f20b7eed6dc109d695a927b2aecdc59250e43b931b4a71ff5b8a1c69682
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
