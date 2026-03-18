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
x64mac=200d919c2f7c1eb7ff52a9e7baa4a29b17efea1cd2db0e3b3aa787c9157a0951
x64win=b59b88176a8d224ae3db4ecddc43e72017adf97288d94cd82f77d5c902dd1ac7
x64freebsd=27260757da068fca4c692defc8f7bba5a1100d4c57b25ba8dc9c3dba74e2034c
x64openbsd=27260757da068fca4c692defc8f7bba5a1100d4c57b25ba8dc9c3dba74e2034c
x64netbsd=27260757da068fca4c692defc8f7bba5a1100d4c57b25ba8dc9c3dba74e2034c
x64musl=27260757da068fca4c692defc8f7bba5a1100d4c57b25ba8dc9c3dba74e2034c
x64glibc=27260757da068fca4c692defc8f7bba5a1100d4c57b25ba8dc9c3dba74e2034c
x64linux=27260757da068fca4c692defc8f7bba5a1100d4c57b25ba8dc9c3dba74e2034c
x64elf=27260757da068fca4c692defc8f7bba5a1100d4c57b25ba8dc9c3dba74e2034c
arm64mac=e2be3171091b9aedae85cc39f83580dde8bf17441fa1dc45c2da57aacc400fe8
arm64win=ab4b38bef3e50d1d2e1e9c53b031a8cc672be082b75b13abf8524f60cf2e2307
arm64linux=b912a507d6519ed212aa7c42ca068356a7301b0547679cdcb636857e253e9293
arm64musl=b912a507d6519ed212aa7c42ca068356a7301b0547679cdcb636857e253e9293
arm64glibc=b912a507d6519ed212aa7c42ca068356a7301b0547679cdcb636857e253e9293
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
