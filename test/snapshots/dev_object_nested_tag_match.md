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
            x64glibc: { files: [app] },
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
	Err(Exit(code)) => code
	Err(_) => -1
}
main = inspect(extract_code(Err(Exit(42))))

~~~
# DEV OUTPUT
~~~ini
x64mac=0b90932a21c83e80700184ee4392f54c3db927ce3560120fac91101f5cda1017
x64win=a85c32d8b5921ce20f69261e1efe1cd11485e1ee3634e901a5742a36770b95fc
x64freebsd=35a1850f1c28546bc830186ff707dcf63728aea8d8db43faab16fd733fbffe15
x64openbsd=35a1850f1c28546bc830186ff707dcf63728aea8d8db43faab16fd733fbffe15
x64netbsd=35a1850f1c28546bc830186ff707dcf63728aea8d8db43faab16fd733fbffe15
x64musl=35a1850f1c28546bc830186ff707dcf63728aea8d8db43faab16fd733fbffe15
x64glibc=35a1850f1c28546bc830186ff707dcf63728aea8d8db43faab16fd733fbffe15
x64linux=35a1850f1c28546bc830186ff707dcf63728aea8d8db43faab16fd733fbffe15
x64elf=35a1850f1c28546bc830186ff707dcf63728aea8d8db43faab16fd733fbffe15
arm64mac=de821fb950ccba75941abe86c8fdb31a2f8df823013ccf0dfa90da82ef511e96
arm64win=beb6bac84ad3763a040f89fa3203c12c7ff7673a77dbfab2bd69fbc144b3adf0
arm64linux=7fc4816e86954b21b8299235df363a929419b292988a892a974ed3e52fc2f13e
arm64musl=7fc4816e86954b21b8299235df363a929419b292988a892a974ed3e52fc2f13e
arm64glibc=7fc4816e86954b21b8299235df363a929419b292988a892a974ed3e52fc2f13e
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
