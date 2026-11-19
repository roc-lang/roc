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
x64mac=41d5587da2b4279ea40d3f5cc669ba9bdb2325ae95651a4a13160c130c93b795
x64win=c780419016c10b8ed66f23ea289e49352f63b786390d19d4e2d89f84396eb1d9
x64mingw=c780419016c10b8ed66f23ea289e49352f63b786390d19d4e2d89f84396eb1d9
x64freebsd=1e3a880bbb2565431e6eb4877b69b586ac7bbcfcb82504e2ef682f3658860c4a
x64openbsd=5159cde0ba20bace6a658eff2dcafc6c0de9267218be83a34e47bb76616e93cb
x64netbsd=22a7650c659eef8501ffcf42a81bf9235afdfc50439af986fcfe2f63ca3d43b3
x64musl=22a7650c659eef8501ffcf42a81bf9235afdfc50439af986fcfe2f63ca3d43b3
x64glibc=22a7650c659eef8501ffcf42a81bf9235afdfc50439af986fcfe2f63ca3d43b3
x64linux=22a7650c659eef8501ffcf42a81bf9235afdfc50439af986fcfe2f63ca3d43b3
x64elf=22a7650c659eef8501ffcf42a81bf9235afdfc50439af986fcfe2f63ca3d43b3
x64v1mac=41d5587da2b4279ea40d3f5cc669ba9bdb2325ae95651a4a13160c130c93b795
x64v1win=c780419016c10b8ed66f23ea289e49352f63b786390d19d4e2d89f84396eb1d9
x64v1mingw=c780419016c10b8ed66f23ea289e49352f63b786390d19d4e2d89f84396eb1d9
x64v1freebsd=1e3a880bbb2565431e6eb4877b69b586ac7bbcfcb82504e2ef682f3658860c4a
x64v1openbsd=5159cde0ba20bace6a658eff2dcafc6c0de9267218be83a34e47bb76616e93cb
x64v1netbsd=22a7650c659eef8501ffcf42a81bf9235afdfc50439af986fcfe2f63ca3d43b3
x64v1musl=22a7650c659eef8501ffcf42a81bf9235afdfc50439af986fcfe2f63ca3d43b3
x64v1glibc=22a7650c659eef8501ffcf42a81bf9235afdfc50439af986fcfe2f63ca3d43b3
x64v1linux=22a7650c659eef8501ffcf42a81bf9235afdfc50439af986fcfe2f63ca3d43b3
x64v1elf=22a7650c659eef8501ffcf42a81bf9235afdfc50439af986fcfe2f63ca3d43b3
arm64mac=382a0996174e9bd2bc30d87b04711982b5c553d30ea57a4dd9bdfded36d9b341
arm64win=32d277ecf27c56bb70a2c0120627148c25d5c791ced3a1dbaee7b6e969be1f22
arm64mingw=32d277ecf27c56bb70a2c0120627148c25d5c791ced3a1dbaee7b6e969be1f22
arm64linux=edb540cbd733342b1264f35712c7c3667e70ef228b93c93af1055762d7e48c1b
arm64musl=edb540cbd733342b1264f35712c7c3667e70ef228b93c93af1055762d7e48c1b
arm64glibc=edb540cbd733342b1264f35712c7c3667e70ef228b93c93af1055762d7e48c1b
arm64v1win=32d277ecf27c56bb70a2c0120627148c25d5c791ced3a1dbaee7b6e969be1f22
arm64v1mingw=32d277ecf27c56bb70a2c0120627148c25d5c791ced3a1dbaee7b6e969be1f22
arm64v1linux=edb540cbd733342b1264f35712c7c3667e70ef228b93c93af1055762d7e48c1b
arm64v1musl=edb540cbd733342b1264f35712c7c3667e70ef228b93c93af1055762d7e48c1b
arm64v1glibc=edb540cbd733342b1264f35712c7c3667e70ef228b93c93af1055762d7e48c1b
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
