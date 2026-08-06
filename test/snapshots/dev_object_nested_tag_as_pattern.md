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
x64mac=f11e5566def97530e6f4829771959a0431e0aa30f0985758853534453328ceb8
x64win=8d365f9992f6ec84c377f8f24f1e3ce181badeea199ca57c8b9b1dd4873711bd
x64mingw=8d365f9992f6ec84c377f8f24f1e3ce181badeea199ca57c8b9b1dd4873711bd
x64freebsd=8a3ded0449898638cae6beb99dbb2f6009891f1ae84d3fb4a7212ddcd96536ec
x64openbsd=f69b8e1605439bbe3073d0e3ef27dfe49a5d5e80f71967a4bb083ab8992ecae3
x64netbsd=4bdb86940d48c3657eeb655963bb2f239c4022ce3a1651fb2526ff076d8535bf
x64musl=4bdb86940d48c3657eeb655963bb2f239c4022ce3a1651fb2526ff076d8535bf
x64glibc=4bdb86940d48c3657eeb655963bb2f239c4022ce3a1651fb2526ff076d8535bf
x64linux=4bdb86940d48c3657eeb655963bb2f239c4022ce3a1651fb2526ff076d8535bf
x64elf=4bdb86940d48c3657eeb655963bb2f239c4022ce3a1651fb2526ff076d8535bf
x64v1mac=f11e5566def97530e6f4829771959a0431e0aa30f0985758853534453328ceb8
x64v1win=8d365f9992f6ec84c377f8f24f1e3ce181badeea199ca57c8b9b1dd4873711bd
x64v1mingw=8d365f9992f6ec84c377f8f24f1e3ce181badeea199ca57c8b9b1dd4873711bd
x64v1freebsd=8a3ded0449898638cae6beb99dbb2f6009891f1ae84d3fb4a7212ddcd96536ec
x64v1openbsd=f69b8e1605439bbe3073d0e3ef27dfe49a5d5e80f71967a4bb083ab8992ecae3
x64v1netbsd=4bdb86940d48c3657eeb655963bb2f239c4022ce3a1651fb2526ff076d8535bf
x64v1musl=4bdb86940d48c3657eeb655963bb2f239c4022ce3a1651fb2526ff076d8535bf
x64v1glibc=4bdb86940d48c3657eeb655963bb2f239c4022ce3a1651fb2526ff076d8535bf
x64v1linux=4bdb86940d48c3657eeb655963bb2f239c4022ce3a1651fb2526ff076d8535bf
x64v1elf=4bdb86940d48c3657eeb655963bb2f239c4022ce3a1651fb2526ff076d8535bf
arm64mac=538d650c0e9d36ef89795d8a4abea6d2ad06c7b4c5e8aef58461b6ce339de212
arm64win=11ad42f20af13bfe408f398e9401d00a2ea1354c9f33455eea36892bf981b72b
arm64mingw=11ad42f20af13bfe408f398e9401d00a2ea1354c9f33455eea36892bf981b72b
arm64linux=aa1f2510499fa81f78a7636f4d773c2f2f95f6bcfb9742835868e166449559da
arm64musl=aa1f2510499fa81f78a7636f4d773c2f2f95f6bcfb9742835868e166449559da
arm64glibc=aa1f2510499fa81f78a7636f4d773c2f2f95f6bcfb9742835868e166449559da
arm64v1win=11ad42f20af13bfe408f398e9401d00a2ea1354c9f33455eea36892bf981b72b
arm64v1mingw=11ad42f20af13bfe408f398e9401d00a2ea1354c9f33455eea36892bf981b72b
arm64v1linux=aa1f2510499fa81f78a7636f4d773c2f2f95f6bcfb9742835868e166449559da
arm64v1musl=aa1f2510499fa81f78a7636f4d773c2f2f95f6bcfb9742835868e166449559da
arm64v1glibc=aa1f2510499fa81f78a7636f4d773c2f2f95f6bcfb9742835868e166449559da
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
