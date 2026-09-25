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
x64mac=6b0dbdb1b35d206560d91cedc49de9a7f4e7efbd1d9366dd97c93667722c81d0
x64win=be5ce5cbec56addfcb7a41ec07405e6116cf10e7601017e72b59e59ec5f4e3df
x64mingw=be5ce5cbec56addfcb7a41ec07405e6116cf10e7601017e72b59e59ec5f4e3df
x64freebsd=b94947e5a3710b5e178e010b3be512a19733825a762de9acabd994160c5efd26
x64openbsd=dee105202c834b0adda5f95e3fca65b6df7628c34c86d826627dbc16c9cd1bae
x64netbsd=db95086b5400c8107d6c84e432da34a3fe4ff58d94597808adf0a0bde41a619d
x64musl=db95086b5400c8107d6c84e432da34a3fe4ff58d94597808adf0a0bde41a619d
x64glibc=db95086b5400c8107d6c84e432da34a3fe4ff58d94597808adf0a0bde41a619d
x64linux=db95086b5400c8107d6c84e432da34a3fe4ff58d94597808adf0a0bde41a619d
x64elf=db95086b5400c8107d6c84e432da34a3fe4ff58d94597808adf0a0bde41a619d
x64v1mac=6b0dbdb1b35d206560d91cedc49de9a7f4e7efbd1d9366dd97c93667722c81d0
x64v1win=be5ce5cbec56addfcb7a41ec07405e6116cf10e7601017e72b59e59ec5f4e3df
x64v1mingw=be5ce5cbec56addfcb7a41ec07405e6116cf10e7601017e72b59e59ec5f4e3df
x64v1freebsd=b94947e5a3710b5e178e010b3be512a19733825a762de9acabd994160c5efd26
x64v1openbsd=dee105202c834b0adda5f95e3fca65b6df7628c34c86d826627dbc16c9cd1bae
x64v1netbsd=db95086b5400c8107d6c84e432da34a3fe4ff58d94597808adf0a0bde41a619d
x64v1musl=db95086b5400c8107d6c84e432da34a3fe4ff58d94597808adf0a0bde41a619d
x64v1glibc=db95086b5400c8107d6c84e432da34a3fe4ff58d94597808adf0a0bde41a619d
x64v1linux=db95086b5400c8107d6c84e432da34a3fe4ff58d94597808adf0a0bde41a619d
x64v1elf=db95086b5400c8107d6c84e432da34a3fe4ff58d94597808adf0a0bde41a619d
arm64mac=65d3538f6dd5bba309331464acbd1614930f1f1d82c00e9835f83888ee5fa737
arm64win=05f72217b2c5d278658d2cc636d0ad5e0a8b7faf919ca774c899c05529441285
arm64mingw=05f72217b2c5d278658d2cc636d0ad5e0a8b7faf919ca774c899c05529441285
arm64linux=03944da5573fafb9dbaf8522a7993612d341b98f77b9f5eaff4e8d9c93d2ae88
arm64musl=03944da5573fafb9dbaf8522a7993612d341b98f77b9f5eaff4e8d9c93d2ae88
arm64glibc=03944da5573fafb9dbaf8522a7993612d341b98f77b9f5eaff4e8d9c93d2ae88
arm64v1win=05f72217b2c5d278658d2cc636d0ad5e0a8b7faf919ca774c899c05529441285
arm64v1mingw=05f72217b2c5d278658d2cc636d0ad5e0a8b7faf919ca774c899c05529441285
arm64v1linux=03944da5573fafb9dbaf8522a7993612d341b98f77b9f5eaff4e8d9c93d2ae88
arm64v1musl=03944da5573fafb9dbaf8522a7993612d341b98f77b9f5eaff4e8d9c93d2ae88
arm64v1glibc=03944da5573fafb9dbaf8522a7993612d341b98f77b9f5eaff4e8d9c93d2ae88
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
