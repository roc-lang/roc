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
x64mac=32ca1b1d1f12b766cb19aacffb6d2f3e00a3d57391822cee27574d7d5a92d877
x64win=a85c32d8b5921ce20f69261e1efe1cd11485e1ee3634e901a5742a36770b95fc
x64freebsd=473041a173746e9f13438c1d0221eb0f5c28697724974950053c3513922f9f5b
x64openbsd=473041a173746e9f13438c1d0221eb0f5c28697724974950053c3513922f9f5b
x64netbsd=473041a173746e9f13438c1d0221eb0f5c28697724974950053c3513922f9f5b
x64musl=473041a173746e9f13438c1d0221eb0f5c28697724974950053c3513922f9f5b
x64glibc=473041a173746e9f13438c1d0221eb0f5c28697724974950053c3513922f9f5b
x64linux=473041a173746e9f13438c1d0221eb0f5c28697724974950053c3513922f9f5b
x64elf=473041a173746e9f13438c1d0221eb0f5c28697724974950053c3513922f9f5b
arm64mac=19b415661a641363b96933938346cda4d842e5dc94438d7699e16a34bef57236
arm64win=beb6bac84ad3763a040f89fa3203c12c7ff7673a77dbfab2bd69fbc144b3adf0
arm64linux=84cf251c791754a4bcacfe3d910e60751a7a8e5830f6c613d06ebd0b880a6e29
arm64musl=84cf251c791754a4bcacfe3d910e60751a7a8e5830f6c613d06ebd0b880a6e29
arm64glibc=84cf251c791754a4bcacfe3d910e60751a7a8e5830f6c613d06ebd0b880a6e29
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
