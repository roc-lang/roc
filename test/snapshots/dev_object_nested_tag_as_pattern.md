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

# platform
main_for_host = <required>

~~~
# DEV OUTPUT
~~~ini
x64mac=1b699f48c65002b6d96e1fa6266d2cb4abb0a95807975baeb8f5bab19e7119e5
x64win=8c7ea6f13a9dc392b3b4af4d9e3508685dff8959437d5e837552219735e8f772
x64freebsd=d382b0a82575d2542dfbcb6c19fcf67e0f90dfdc84cc721a13a030325f7dea10
x64openbsd=d382b0a82575d2542dfbcb6c19fcf67e0f90dfdc84cc721a13a030325f7dea10
x64netbsd=d382b0a82575d2542dfbcb6c19fcf67e0f90dfdc84cc721a13a030325f7dea10
x64musl=d382b0a82575d2542dfbcb6c19fcf67e0f90dfdc84cc721a13a030325f7dea10
x64glibc=d382b0a82575d2542dfbcb6c19fcf67e0f90dfdc84cc721a13a030325f7dea10
x64linux=d382b0a82575d2542dfbcb6c19fcf67e0f90dfdc84cc721a13a030325f7dea10
x64elf=d382b0a82575d2542dfbcb6c19fcf67e0f90dfdc84cc721a13a030325f7dea10
arm64mac=6e0a67df1ac82b6e9584d70c580c14e2069d83906b29041956cc1661add1f194
arm64win=c1a92aea83d5da61c6f0d2babaae1fc30da7f83456b0a494aa2c3aea4c071a7b
arm64linux=f18d2071801ee36e665404405e747a0ee127453c4f42ca4708898a67cdc4aadf
arm64musl=f18d2071801ee36e665404405e747a0ee127453c4f42ca4708898a67cdc4aadf
arm64glibc=f18d2071801ee36e665404405e747a0ee127453c4f42ca4708898a67cdc4aadf
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
