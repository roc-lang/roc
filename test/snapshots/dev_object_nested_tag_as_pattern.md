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
x64mac=ce2e671354c73ec72c29a2d7738f0ef484086fc0f46a0ddadfb1b05bd1052277
x64win=5b8fea5c3c0f7f0e1a8ed774d2dac9b9d8e34506121190f437b4e4574b96385e
x64freebsd=40999167f51ec2f2634021848c1382077aff443a5a17880c7fd4951ac6ab1992
x64openbsd=40999167f51ec2f2634021848c1382077aff443a5a17880c7fd4951ac6ab1992
x64netbsd=40999167f51ec2f2634021848c1382077aff443a5a17880c7fd4951ac6ab1992
x64musl=40999167f51ec2f2634021848c1382077aff443a5a17880c7fd4951ac6ab1992
x64glibc=40999167f51ec2f2634021848c1382077aff443a5a17880c7fd4951ac6ab1992
x64linux=40999167f51ec2f2634021848c1382077aff443a5a17880c7fd4951ac6ab1992
x64elf=40999167f51ec2f2634021848c1382077aff443a5a17880c7fd4951ac6ab1992
arm64mac=15920ffdaf583129cdd7e41d5407466093d1686e7d0185f9b517ff6af4a719f4
arm64win=31dc998c3054b9c91b9493b3c7a68230d69c204e513959b3e86d0b4ca4a21157
arm64linux=e5f3b4da3ddade095ab04cebc01f44ef2ce32746b9ab3ef18a906ad362357a45
arm64musl=e5f3b4da3ddade095ab04cebc01f44ef2ce32746b9ab3ef18a906ad362357a45
arm64glibc=e5f3b4da3ddade095ab04cebc01f44ef2ce32746b9ab3ef18a906ad362357a45
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
