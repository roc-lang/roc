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
    provides { main_for_host: "main" }
    targets: {
        files: "targets/",
        exe: {
            x64glibc: [app],
        }
    }

main_for_host : Str
main_for_host = main
~~~
# MONO
~~~roc
# app
extract_code = |result| match result {
	Ok(n) => n
	Err(Exit(code)) => code
	Err(_) => -1
}
main = inspect(extract_code(Err(Exit(42))))

# platform
main_for_host = <required>

~~~
# DEV OUTPUT
~~~ini
x64mac=9ea7fb5ad37c00a24605be77505d0a3fe17bf7ece78d2eb09116b6bc509c11b2
x64win=2c0fe55562918443cdb434e5bb4ed2fcad55ad4ed78810a8cfbddf359cdafaa2
x64freebsd=8317834c52c53cefe7c6622274bfa25de454b190f43f49a90e9b94f87c84f69c
x64openbsd=8317834c52c53cefe7c6622274bfa25de454b190f43f49a90e9b94f87c84f69c
x64netbsd=8317834c52c53cefe7c6622274bfa25de454b190f43f49a90e9b94f87c84f69c
x64musl=8317834c52c53cefe7c6622274bfa25de454b190f43f49a90e9b94f87c84f69c
x64glibc=8317834c52c53cefe7c6622274bfa25de454b190f43f49a90e9b94f87c84f69c
x64linux=8317834c52c53cefe7c6622274bfa25de454b190f43f49a90e9b94f87c84f69c
x64elf=8317834c52c53cefe7c6622274bfa25de454b190f43f49a90e9b94f87c84f69c
arm64mac=8849f50d4dbb5cc3110ab552480b41cb73b46f8dbc2943e1ac064af57fc9e034
arm64win=8b44ddad6bdb235c52797b0aa1195b9e55c053a6acb1f5974b21e831dcb174d5
arm64linux=32d51fe67c12f47ceff5cb49c7a171b42839323aa116b525e3565629d22dec81
arm64musl=32d51fe67c12f47ceff5cb49c7a171b42839323aa116b525e3565629d22dec81
arm64glibc=32d51fe67c12f47ceff5cb49c7a171b42839323aa116b525e3565629d22dec81
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
