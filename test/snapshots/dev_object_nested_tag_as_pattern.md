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
x64mac=23ea4680ffdff4907414f9930b4add9f9fd9053e8f2e939dc39001bf9dd2d6e9
x64win=8c5d80b85acaddf711276591305d30d93cf65b3935b338bece5877b6b210f3a1
x64mingw=8c5d80b85acaddf711276591305d30d93cf65b3935b338bece5877b6b210f3a1
x64freebsd=20ed031a395745d631c8b6d5372e8ccef7343048c51d7f7ca397bdb4b10cee4f
x64openbsd=388729990f648481b4247fd02b7c3e26864bf81e8e1e0aa7a9a005d32d6fe393
x64netbsd=b1d1d1bc156305346b17c18060926b392e19ee1abbc8df4e666773b0bf4d825e
x64musl=b1d1d1bc156305346b17c18060926b392e19ee1abbc8df4e666773b0bf4d825e
x64glibc=b1d1d1bc156305346b17c18060926b392e19ee1abbc8df4e666773b0bf4d825e
x64linux=b1d1d1bc156305346b17c18060926b392e19ee1abbc8df4e666773b0bf4d825e
x64elf=b1d1d1bc156305346b17c18060926b392e19ee1abbc8df4e666773b0bf4d825e
x64v1mac=23ea4680ffdff4907414f9930b4add9f9fd9053e8f2e939dc39001bf9dd2d6e9
x64v1win=8c5d80b85acaddf711276591305d30d93cf65b3935b338bece5877b6b210f3a1
x64v1mingw=8c5d80b85acaddf711276591305d30d93cf65b3935b338bece5877b6b210f3a1
x64v1freebsd=20ed031a395745d631c8b6d5372e8ccef7343048c51d7f7ca397bdb4b10cee4f
x64v1openbsd=388729990f648481b4247fd02b7c3e26864bf81e8e1e0aa7a9a005d32d6fe393
x64v1netbsd=b1d1d1bc156305346b17c18060926b392e19ee1abbc8df4e666773b0bf4d825e
x64v1musl=b1d1d1bc156305346b17c18060926b392e19ee1abbc8df4e666773b0bf4d825e
x64v1glibc=b1d1d1bc156305346b17c18060926b392e19ee1abbc8df4e666773b0bf4d825e
x64v1linux=b1d1d1bc156305346b17c18060926b392e19ee1abbc8df4e666773b0bf4d825e
x64v1elf=b1d1d1bc156305346b17c18060926b392e19ee1abbc8df4e666773b0bf4d825e
arm64mac=11fc74458408a6b7331453bdb53eb370143ec9b44ecd679b23715218a7cf664a
arm64win=eff0e895ea34a70b5119e05da9f8eae54be537c3738253fd09aebc9b5db3f7c4
arm64mingw=eff0e895ea34a70b5119e05da9f8eae54be537c3738253fd09aebc9b5db3f7c4
arm64linux=debe3c2d91fae1be471268232cac9284fb0b1fbb76657b15817f9dca21a9a9af
arm64musl=debe3c2d91fae1be471268232cac9284fb0b1fbb76657b15817f9dca21a9a9af
arm64glibc=debe3c2d91fae1be471268232cac9284fb0b1fbb76657b15817f9dca21a9a9af
arm64v1win=eff0e895ea34a70b5119e05da9f8eae54be537c3738253fd09aebc9b5db3f7c4
arm64v1mingw=eff0e895ea34a70b5119e05da9f8eae54be537c3738253fd09aebc9b5db3f7c4
arm64v1linux=debe3c2d91fae1be471268232cac9284fb0b1fbb76657b15817f9dca21a9a9af
arm64v1musl=debe3c2d91fae1be471268232cac9284fb0b1fbb76657b15817f9dca21a9a9af
arm64v1glibc=debe3c2d91fae1be471268232cac9284fb0b1fbb76657b15817f9dca21a9a9af
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
