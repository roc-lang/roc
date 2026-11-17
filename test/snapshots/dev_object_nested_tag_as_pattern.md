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
x64mac=7657b6dc22672cfb9aa1e1053955a39513bd7c2f7e454f57ba27d7ed506db615
x64win=463b0bf98b73dcca4258152d38c061ddbac1432b7c1655f570bc8b1edec27f81
x64mingw=463b0bf98b73dcca4258152d38c061ddbac1432b7c1655f570bc8b1edec27f81
x64freebsd=ff751ee3e2fce1b0a451373c296aedc6593d124f2827cfc888a420f5cf7d1a9b
x64openbsd=0d32c782f6483b43be97dec8ab78f42f11890a1b6e653ef06ef9d4629937ac59
x64netbsd=914f7bb28f663cb6fb7371fc198201823cfaa1f2f590a12020ab98a99a78a9f1
x64musl=914f7bb28f663cb6fb7371fc198201823cfaa1f2f590a12020ab98a99a78a9f1
x64glibc=914f7bb28f663cb6fb7371fc198201823cfaa1f2f590a12020ab98a99a78a9f1
x64linux=914f7bb28f663cb6fb7371fc198201823cfaa1f2f590a12020ab98a99a78a9f1
x64elf=914f7bb28f663cb6fb7371fc198201823cfaa1f2f590a12020ab98a99a78a9f1
x64v1mac=7657b6dc22672cfb9aa1e1053955a39513bd7c2f7e454f57ba27d7ed506db615
x64v1win=463b0bf98b73dcca4258152d38c061ddbac1432b7c1655f570bc8b1edec27f81
x64v1mingw=463b0bf98b73dcca4258152d38c061ddbac1432b7c1655f570bc8b1edec27f81
x64v1freebsd=ff751ee3e2fce1b0a451373c296aedc6593d124f2827cfc888a420f5cf7d1a9b
x64v1openbsd=0d32c782f6483b43be97dec8ab78f42f11890a1b6e653ef06ef9d4629937ac59
x64v1netbsd=914f7bb28f663cb6fb7371fc198201823cfaa1f2f590a12020ab98a99a78a9f1
x64v1musl=914f7bb28f663cb6fb7371fc198201823cfaa1f2f590a12020ab98a99a78a9f1
x64v1glibc=914f7bb28f663cb6fb7371fc198201823cfaa1f2f590a12020ab98a99a78a9f1
x64v1linux=914f7bb28f663cb6fb7371fc198201823cfaa1f2f590a12020ab98a99a78a9f1
x64v1elf=914f7bb28f663cb6fb7371fc198201823cfaa1f2f590a12020ab98a99a78a9f1
arm64mac=2f494d803f39720723c807be029c152c69d5d9db082e3de4cffbf5a93b40b5c9
arm64win=0cc16e29eb9420d931cee0cfec8167b05848474d5b1f79db4e0dbedeb0ee3bb6
arm64mingw=0cc16e29eb9420d931cee0cfec8167b05848474d5b1f79db4e0dbedeb0ee3bb6
arm64linux=3b542929767b3ef76dcdcde15848a82af35827369c8d4fb46730abbd5e82a0cb
arm64musl=3b542929767b3ef76dcdcde15848a82af35827369c8d4fb46730abbd5e82a0cb
arm64glibc=3b542929767b3ef76dcdcde15848a82af35827369c8d4fb46730abbd5e82a0cb
arm64v1win=0cc16e29eb9420d931cee0cfec8167b05848474d5b1f79db4e0dbedeb0ee3bb6
arm64v1mingw=0cc16e29eb9420d931cee0cfec8167b05848474d5b1f79db4e0dbedeb0ee3bb6
arm64v1linux=3b542929767b3ef76dcdcde15848a82af35827369c8d4fb46730abbd5e82a0cb
arm64v1musl=3b542929767b3ef76dcdcde15848a82af35827369c8d4fb46730abbd5e82a0cb
arm64v1glibc=3b542929767b3ef76dcdcde15848a82af35827369c8d4fb46730abbd5e82a0cb
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
