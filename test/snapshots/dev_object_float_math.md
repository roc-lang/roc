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
x64mac=297ee5fe4d191cca9b4162222fbc713af69cb1da6e7a4b4957f193c0ac4033c3
x64win=0bed30653807fe9c2925a4d4a83b7777379142b6b762a6c199611dfe106acac3
x64mingw=0bed30653807fe9c2925a4d4a83b7777379142b6b762a6c199611dfe106acac3
x64freebsd=f0ed81026a39ac2fc7b95c2f1759f79e1067d0fe51f207983e9f84ff1182d1da
x64openbsd=31898eb946345bdb139b9126c9b443fedbd1a96d85fde7c54e26a10badaf60b9
x64netbsd=7c7d566b593f8d126f13f17ba72491a2071275a4c743dafa0a6da2432065da79
x64musl=7c7d566b593f8d126f13f17ba72491a2071275a4c743dafa0a6da2432065da79
x64glibc=7c7d566b593f8d126f13f17ba72491a2071275a4c743dafa0a6da2432065da79
x64linux=7c7d566b593f8d126f13f17ba72491a2071275a4c743dafa0a6da2432065da79
x64elf=7c7d566b593f8d126f13f17ba72491a2071275a4c743dafa0a6da2432065da79
x64v1mac=297ee5fe4d191cca9b4162222fbc713af69cb1da6e7a4b4957f193c0ac4033c3
x64v1win=0bed30653807fe9c2925a4d4a83b7777379142b6b762a6c199611dfe106acac3
x64v1mingw=0bed30653807fe9c2925a4d4a83b7777379142b6b762a6c199611dfe106acac3
x64v1freebsd=f0ed81026a39ac2fc7b95c2f1759f79e1067d0fe51f207983e9f84ff1182d1da
x64v1openbsd=31898eb946345bdb139b9126c9b443fedbd1a96d85fde7c54e26a10badaf60b9
x64v1netbsd=7c7d566b593f8d126f13f17ba72491a2071275a4c743dafa0a6da2432065da79
x64v1musl=7c7d566b593f8d126f13f17ba72491a2071275a4c743dafa0a6da2432065da79
x64v1glibc=7c7d566b593f8d126f13f17ba72491a2071275a4c743dafa0a6da2432065da79
x64v1linux=7c7d566b593f8d126f13f17ba72491a2071275a4c743dafa0a6da2432065da79
x64v1elf=7c7d566b593f8d126f13f17ba72491a2071275a4c743dafa0a6da2432065da79
arm64mac=b352fd772b1e5ea381958d710ad12835b1638c2c34e26e4c25bc24fe061bf328
arm64win=9e17b5b962ccdb73c8c4e9711c82186837ccf8499c555ba069567110a78ad440
arm64mingw=9e17b5b962ccdb73c8c4e9711c82186837ccf8499c555ba069567110a78ad440
arm64linux=0a3834f074ce133fe24ba235bd5db4c316eb3cae9a05eaba7d5fc6244ffeaf8e
arm64musl=0a3834f074ce133fe24ba235bd5db4c316eb3cae9a05eaba7d5fc6244ffeaf8e
arm64glibc=0a3834f074ce133fe24ba235bd5db4c316eb3cae9a05eaba7d5fc6244ffeaf8e
arm64v1win=9e17b5b962ccdb73c8c4e9711c82186837ccf8499c555ba069567110a78ad440
arm64v1mingw=9e17b5b962ccdb73c8c4e9711c82186837ccf8499c555ba069567110a78ad440
arm64v1linux=0a3834f074ce133fe24ba235bd5db4c316eb3cae9a05eaba7d5fc6244ffeaf8e
arm64v1musl=0a3834f074ce133fe24ba235bd5db4c316eb3cae9a05eaba7d5fc6244ffeaf8e
arm64v1glibc=0a3834f074ce133fe24ba235bd5db4c316eb3cae9a05eaba7d5fc6244ffeaf8e
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
