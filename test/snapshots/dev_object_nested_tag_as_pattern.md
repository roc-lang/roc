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
x64mac=cd6e9b5fa0957f0baba36f81407aac7eeeb1208141a4fc4b900ae2aa95c6bb1b
x64win=8d365f9992f6ec84c377f8f24f1e3ce181badeea199ca57c8b9b1dd4873711bd
x64freebsd=0cff47fef0f78332d9613f81ea24a70be9565ce829232c24e7a7ae311d6fa87e
x64openbsd=0cff47fef0f78332d9613f81ea24a70be9565ce829232c24e7a7ae311d6fa87e
x64netbsd=0cff47fef0f78332d9613f81ea24a70be9565ce829232c24e7a7ae311d6fa87e
x64musl=0cff47fef0f78332d9613f81ea24a70be9565ce829232c24e7a7ae311d6fa87e
x64glibc=0cff47fef0f78332d9613f81ea24a70be9565ce829232c24e7a7ae311d6fa87e
x64linux=0cff47fef0f78332d9613f81ea24a70be9565ce829232c24e7a7ae311d6fa87e
x64elf=0cff47fef0f78332d9613f81ea24a70be9565ce829232c24e7a7ae311d6fa87e
arm64mac=7e49b8281b00345bffe5eeb8b4ab51a7fe694adb90785c982a391a1d955729fe
arm64win=11ad42f20af13bfe408f398e9401d00a2ea1354c9f33455eea36892bf981b72b
arm64linux=6599fd715945b61b9143502418e76f603f66379cbaa6e23a44cd1bf905289ca4
arm64musl=6599fd715945b61b9143502418e76f603f66379cbaa6e23a44cd1bf905289ca4
arm64glibc=6599fd715945b61b9143502418e76f603f66379cbaa6e23a44cd1bf905289ca4
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
