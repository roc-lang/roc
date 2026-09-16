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
x64mac=e0d71d3fcdf24645640c70d0bb2b163c3b410ccd4817940dd0c26b3c9d361115
x64win=fb5ee7ea1260b64efc8d5464d3ac4307b9bb50fb2e709c9a20ce4bf212d2b2af
x64mingw=fb5ee7ea1260b64efc8d5464d3ac4307b9bb50fb2e709c9a20ce4bf212d2b2af
x64freebsd=d37b24fd32ec3e7db80324d1a48a65a0db4befec2d045bca765779aaf9abae51
x64openbsd=58522fbd6b4db2c98d16d38412713a8f087795c01afa46ca2af0daee6a083a37
x64netbsd=862f7f8cc5133f4927e15ab1d73181ff2919b9b80afbb37373db8a7b1febad87
x64musl=862f7f8cc5133f4927e15ab1d73181ff2919b9b80afbb37373db8a7b1febad87
x64glibc=862f7f8cc5133f4927e15ab1d73181ff2919b9b80afbb37373db8a7b1febad87
x64linux=862f7f8cc5133f4927e15ab1d73181ff2919b9b80afbb37373db8a7b1febad87
x64elf=862f7f8cc5133f4927e15ab1d73181ff2919b9b80afbb37373db8a7b1febad87
x64v1mac=e0d71d3fcdf24645640c70d0bb2b163c3b410ccd4817940dd0c26b3c9d361115
x64v1win=fb5ee7ea1260b64efc8d5464d3ac4307b9bb50fb2e709c9a20ce4bf212d2b2af
x64v1mingw=fb5ee7ea1260b64efc8d5464d3ac4307b9bb50fb2e709c9a20ce4bf212d2b2af
x64v1freebsd=d37b24fd32ec3e7db80324d1a48a65a0db4befec2d045bca765779aaf9abae51
x64v1openbsd=58522fbd6b4db2c98d16d38412713a8f087795c01afa46ca2af0daee6a083a37
x64v1netbsd=862f7f8cc5133f4927e15ab1d73181ff2919b9b80afbb37373db8a7b1febad87
x64v1musl=862f7f8cc5133f4927e15ab1d73181ff2919b9b80afbb37373db8a7b1febad87
x64v1glibc=862f7f8cc5133f4927e15ab1d73181ff2919b9b80afbb37373db8a7b1febad87
x64v1linux=862f7f8cc5133f4927e15ab1d73181ff2919b9b80afbb37373db8a7b1febad87
x64v1elf=862f7f8cc5133f4927e15ab1d73181ff2919b9b80afbb37373db8a7b1febad87
arm64mac=f972aa3d0742e9c94a4ab79d3bbd1c84526bd377ee6f4ba021f587ba6a504628
arm64win=63ad8adadb69b7eeac02a6b3f2a541b998d5ea932994d745f432bef551a1dce7
arm64mingw=63ad8adadb69b7eeac02a6b3f2a541b998d5ea932994d745f432bef551a1dce7
arm64linux=3b44a6069d3fba1105c288fbf0d06aa425ab2e137119b36b81465c7609411397
arm64musl=3b44a6069d3fba1105c288fbf0d06aa425ab2e137119b36b81465c7609411397
arm64glibc=3b44a6069d3fba1105c288fbf0d06aa425ab2e137119b36b81465c7609411397
arm64v1win=63ad8adadb69b7eeac02a6b3f2a541b998d5ea932994d745f432bef551a1dce7
arm64v1mingw=63ad8adadb69b7eeac02a6b3f2a541b998d5ea932994d745f432bef551a1dce7
arm64v1linux=3b44a6069d3fba1105c288fbf0d06aa425ab2e137119b36b81465c7609411397
arm64v1musl=3b44a6069d3fba1105c288fbf0d06aa425ab2e137119b36b81465c7609411397
arm64v1glibc=3b44a6069d3fba1105c288fbf0d06aa425ab2e137119b36b81465c7609411397
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
