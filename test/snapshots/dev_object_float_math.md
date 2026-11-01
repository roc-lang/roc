# META
~~~ini
description=F32/F64 arithmetic, comparisons, and F64 to I64 conversion in host-called functions
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

main : F64, F32, I64 -> I64
main = |x, y, n| {
    scaled = x * 2.5 - I64.to_f64(n) / 3.0
    narrowed = F32.to_f64(y * 1.5 + 0.25)
    total = if scaled > narrowed { scaled - narrowed } else { narrowed + scaled }
    F64.to_i64_wrap(total)
}
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : F64, F32, I64 -> I64 }
    exposes []
    packages {}
    provides { "roc_main": main_for_host }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
    }

main_for_host : F64, F32, I64 -> I64
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# app
main = |x, y, n| {
	scaled = x * 2.5 - to_f64(n) / 3
	narrowed = to_f64(y * 1.5 + 0.25)
	total = if (scaled > narrowed) {
		scaled - narrowed
	} else {
		narrowed + scaled
	}
	to_i64_wrap(total)
}

~~~
# DEV OUTPUT
~~~ini
x64mac=a7f501fdac1a5b2b0166a715c0199352f30472f400069804cf3d15bc38880d66
x64win=9eec1b5ae590a39f7955896c652170efb33b387f2026591c7b24b775900d3a14
x64mingw=9eec1b5ae590a39f7955896c652170efb33b387f2026591c7b24b775900d3a14
x64freebsd=9957b2387a7dfc0fec257e3403fd60b69780576351b49aae65f84ff7362b21ab
x64openbsd=9f2dc5a078a01840514b3f125b7dc14063c416561dd7db3a68397672c56e1e03
x64netbsd=eff11e8b7d710fe286513190c07cacf6c85c539e63f327984d299b0e933c7ece
x64musl=eff11e8b7d710fe286513190c07cacf6c85c539e63f327984d299b0e933c7ece
x64glibc=eff11e8b7d710fe286513190c07cacf6c85c539e63f327984d299b0e933c7ece
x64linux=eff11e8b7d710fe286513190c07cacf6c85c539e63f327984d299b0e933c7ece
x64elf=eff11e8b7d710fe286513190c07cacf6c85c539e63f327984d299b0e933c7ece
x64v1mac=a7f501fdac1a5b2b0166a715c0199352f30472f400069804cf3d15bc38880d66
x64v1win=9eec1b5ae590a39f7955896c652170efb33b387f2026591c7b24b775900d3a14
x64v1mingw=9eec1b5ae590a39f7955896c652170efb33b387f2026591c7b24b775900d3a14
x64v1freebsd=9957b2387a7dfc0fec257e3403fd60b69780576351b49aae65f84ff7362b21ab
x64v1openbsd=9f2dc5a078a01840514b3f125b7dc14063c416561dd7db3a68397672c56e1e03
x64v1netbsd=eff11e8b7d710fe286513190c07cacf6c85c539e63f327984d299b0e933c7ece
x64v1musl=eff11e8b7d710fe286513190c07cacf6c85c539e63f327984d299b0e933c7ece
x64v1glibc=eff11e8b7d710fe286513190c07cacf6c85c539e63f327984d299b0e933c7ece
x64v1linux=eff11e8b7d710fe286513190c07cacf6c85c539e63f327984d299b0e933c7ece
x64v1elf=eff11e8b7d710fe286513190c07cacf6c85c539e63f327984d299b0e933c7ece
arm64mac=af2f29e8eea855b478f485e54bef0a4ab5453f91b7e419199456efbdf2585064
arm64win=d6ef3d9594492fdef7733d95ac2a5b420105a38e1be0ec6d0b089356071da9d2
arm64mingw=d6ef3d9594492fdef7733d95ac2a5b420105a38e1be0ec6d0b089356071da9d2
arm64linux=3e36390e2514051d5b1c72dba6dfff0f313654ae26b1d3a6efc81cd888122410
arm64musl=3e36390e2514051d5b1c72dba6dfff0f313654ae26b1d3a6efc81cd888122410
arm64glibc=3e36390e2514051d5b1c72dba6dfff0f313654ae26b1d3a6efc81cd888122410
arm64v1win=d6ef3d9594492fdef7733d95ac2a5b420105a38e1be0ec6d0b089356071da9d2
arm64v1mingw=d6ef3d9594492fdef7733d95ac2a5b420105a38e1be0ec6d0b089356071da9d2
arm64v1linux=3e36390e2514051d5b1c72dba6dfff0f313654ae26b1d3a6efc81cd888122410
arm64v1musl=3e36390e2514051d5b1c72dba6dfff0f313654ae26b1d3a6efc81cd888122410
arm64v1glibc=3e36390e2514051d5b1c72dba6dfff0f313654ae26b1d3a6efc81cd888122410
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
