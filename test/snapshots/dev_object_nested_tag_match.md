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
        inputs: "targets/",
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
	Err(Exit(code)) => code
	Err(_) => -1
}
main = inspect(extract_code(Err(Exit(42))))

~~~
# DEV OUTPUT
~~~ini
x64mac=fa575d304941045802c8401f91c3aa06ed066a984347446615e391463dd5cde6
x64win=a85c32d8b5921ce20f69261e1efe1cd11485e1ee3634e901a5742a36770b95fc
x64freebsd=bde79cfbe572c39ee6c5f32a8a702c17c85ac99ca264e1cb5fa01114e0eedcfe
x64openbsd=bde79cfbe572c39ee6c5f32a8a702c17c85ac99ca264e1cb5fa01114e0eedcfe
x64netbsd=bde79cfbe572c39ee6c5f32a8a702c17c85ac99ca264e1cb5fa01114e0eedcfe
x64musl=bde79cfbe572c39ee6c5f32a8a702c17c85ac99ca264e1cb5fa01114e0eedcfe
x64glibc=bde79cfbe572c39ee6c5f32a8a702c17c85ac99ca264e1cb5fa01114e0eedcfe
x64linux=bde79cfbe572c39ee6c5f32a8a702c17c85ac99ca264e1cb5fa01114e0eedcfe
x64elf=bde79cfbe572c39ee6c5f32a8a702c17c85ac99ca264e1cb5fa01114e0eedcfe
arm64mac=fbe38d08166890ab73602a28acd7aa5fffdaf3819cdc3b566cfa2def3827c591
arm64win=beb6bac84ad3763a040f89fa3203c12c7ff7673a77dbfab2bd69fbc144b3adf0
arm64linux=b7e0a6c0dbe57e9a378e7031c2e6ee97891991aa2b125e04cedb68f6d17a6428
arm64musl=b7e0a6c0dbe57e9a378e7031c2e6ee97891991aa2b125e04cedb68f6d17a6428
arm64glibc=b7e0a6c0dbe57e9a378e7031c2e6ee97891991aa2b125e04cedb68f6d17a6428
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
