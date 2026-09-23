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
x64mac=61e9cd0566de0c496ceb20ef56bc4c66a3a74b5827036c100672897cd7452bf2
x64win=3d1ac796dbf9e4f92d7decac8f01c7788066f0b5fc0738604eca43e269c816b9
x64mingw=3d1ac796dbf9e4f92d7decac8f01c7788066f0b5fc0738604eca43e269c816b9
x64freebsd=bcf77b373447e7e14ae9729cc210444da7f695d09fd2395b7d4920a755f88f95
x64openbsd=c471e3436b8bf7f2eec50de6a8aceda5da51250db11371bc1abb4b9f1a4fb6be
x64netbsd=e8ee525d73d41bed362255e3d5b9c64bff8272e85dfe8ea69841f67b400a5d90
x64musl=e8ee525d73d41bed362255e3d5b9c64bff8272e85dfe8ea69841f67b400a5d90
x64glibc=e8ee525d73d41bed362255e3d5b9c64bff8272e85dfe8ea69841f67b400a5d90
x64linux=e8ee525d73d41bed362255e3d5b9c64bff8272e85dfe8ea69841f67b400a5d90
x64elf=e8ee525d73d41bed362255e3d5b9c64bff8272e85dfe8ea69841f67b400a5d90
x64v1mac=61e9cd0566de0c496ceb20ef56bc4c66a3a74b5827036c100672897cd7452bf2
x64v1win=3d1ac796dbf9e4f92d7decac8f01c7788066f0b5fc0738604eca43e269c816b9
x64v1mingw=3d1ac796dbf9e4f92d7decac8f01c7788066f0b5fc0738604eca43e269c816b9
x64v1freebsd=bcf77b373447e7e14ae9729cc210444da7f695d09fd2395b7d4920a755f88f95
x64v1openbsd=c471e3436b8bf7f2eec50de6a8aceda5da51250db11371bc1abb4b9f1a4fb6be
x64v1netbsd=e8ee525d73d41bed362255e3d5b9c64bff8272e85dfe8ea69841f67b400a5d90
x64v1musl=e8ee525d73d41bed362255e3d5b9c64bff8272e85dfe8ea69841f67b400a5d90
x64v1glibc=e8ee525d73d41bed362255e3d5b9c64bff8272e85dfe8ea69841f67b400a5d90
x64v1linux=e8ee525d73d41bed362255e3d5b9c64bff8272e85dfe8ea69841f67b400a5d90
x64v1elf=e8ee525d73d41bed362255e3d5b9c64bff8272e85dfe8ea69841f67b400a5d90
arm64mac=fc63d95d84471c5a8be26b0b9a9dcf9f1e9c7948e4a31d42089ae61bd4d4c05b
arm64win=08c0721ceec77ef61e5a8d7421e327b8b33968f25d2f74cb6795e600a95644f0
arm64mingw=08c0721ceec77ef61e5a8d7421e327b8b33968f25d2f74cb6795e600a95644f0
arm64linux=539c11643b487e685323e11872090edb4f212dd7b374bc9c7b14d8f49bed4e17
arm64musl=539c11643b487e685323e11872090edb4f212dd7b374bc9c7b14d8f49bed4e17
arm64glibc=539c11643b487e685323e11872090edb4f212dd7b374bc9c7b14d8f49bed4e17
arm64v1win=08c0721ceec77ef61e5a8d7421e327b8b33968f25d2f74cb6795e600a95644f0
arm64v1mingw=08c0721ceec77ef61e5a8d7421e327b8b33968f25d2f74cb6795e600a95644f0
arm64v1linux=539c11643b487e685323e11872090edb4f212dd7b374bc9c7b14d8f49bed4e17
arm64v1musl=539c11643b487e685323e11872090edb4f212dd7b374bc9c7b14d8f49bed4e17
arm64v1glibc=539c11643b487e685323e11872090edb4f212dd7b374bc9c7b14d8f49bed4e17
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
