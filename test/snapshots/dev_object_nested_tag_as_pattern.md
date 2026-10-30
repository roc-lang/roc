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
x64mac=4d7f1ae5dae5ce426443df4b17a23d28b129af65066dd5af4f8241b291dd7ace
x64win=5e97d9da1f3e341e048057601e84908458a931fca81fef2beed750b6cb1252fc
x64mingw=5e97d9da1f3e341e048057601e84908458a931fca81fef2beed750b6cb1252fc
x64freebsd=def5c52a9d2751d9e630d8c273f083f7e76eb98799ed407ef79feac4786f521a
x64openbsd=2048fb699bf4723939928a0dfd5eaee4d1656d59fbe3430dd464e46f88af4c2f
x64netbsd=c334e2872e0a47f1c218591626f8e4495d1367322ac2e94908d165a19c3a5edc
x64musl=c334e2872e0a47f1c218591626f8e4495d1367322ac2e94908d165a19c3a5edc
x64glibc=c334e2872e0a47f1c218591626f8e4495d1367322ac2e94908d165a19c3a5edc
x64linux=c334e2872e0a47f1c218591626f8e4495d1367322ac2e94908d165a19c3a5edc
x64elf=c334e2872e0a47f1c218591626f8e4495d1367322ac2e94908d165a19c3a5edc
x64v1mac=4d7f1ae5dae5ce426443df4b17a23d28b129af65066dd5af4f8241b291dd7ace
x64v1win=5e97d9da1f3e341e048057601e84908458a931fca81fef2beed750b6cb1252fc
x64v1mingw=5e97d9da1f3e341e048057601e84908458a931fca81fef2beed750b6cb1252fc
x64v1freebsd=def5c52a9d2751d9e630d8c273f083f7e76eb98799ed407ef79feac4786f521a
x64v1openbsd=2048fb699bf4723939928a0dfd5eaee4d1656d59fbe3430dd464e46f88af4c2f
x64v1netbsd=c334e2872e0a47f1c218591626f8e4495d1367322ac2e94908d165a19c3a5edc
x64v1musl=c334e2872e0a47f1c218591626f8e4495d1367322ac2e94908d165a19c3a5edc
x64v1glibc=c334e2872e0a47f1c218591626f8e4495d1367322ac2e94908d165a19c3a5edc
x64v1linux=c334e2872e0a47f1c218591626f8e4495d1367322ac2e94908d165a19c3a5edc
x64v1elf=c334e2872e0a47f1c218591626f8e4495d1367322ac2e94908d165a19c3a5edc
arm64mac=d8459589e489d115b013458b567aa419ea3e7df99cb51b8ec09c3625a79dd16c
arm64win=f3eb06b4c8853835252977fadfe7fb13a4338d131387530ba4e9cbd919ec2dbf
arm64mingw=f3eb06b4c8853835252977fadfe7fb13a4338d131387530ba4e9cbd919ec2dbf
arm64linux=39544747515599c9036673544cf2e7ebb107ff3fcae7dabcfa2a0c924f67cbba
arm64musl=39544747515599c9036673544cf2e7ebb107ff3fcae7dabcfa2a0c924f67cbba
arm64glibc=39544747515599c9036673544cf2e7ebb107ff3fcae7dabcfa2a0c924f67cbba
arm64v1win=f3eb06b4c8853835252977fadfe7fb13a4338d131387530ba4e9cbd919ec2dbf
arm64v1mingw=f3eb06b4c8853835252977fadfe7fb13a4338d131387530ba4e9cbd919ec2dbf
arm64v1linux=39544747515599c9036673544cf2e7ebb107ff3fcae7dabcfa2a0c924f67cbba
arm64v1musl=39544747515599c9036673544cf2e7ebb107ff3fcae7dabcfa2a0c924f67cbba
arm64v1glibc=39544747515599c9036673544cf2e7ebb107ff3fcae7dabcfa2a0c924f67cbba
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
