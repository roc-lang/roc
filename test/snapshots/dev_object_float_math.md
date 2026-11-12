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
x64mac=a005d8d084eef1ff1eb75ea9ca67d1dd0ad2b68c2ed387ec0eea41869450e9bd
x64win=ddd12f80c979a4fc2485ebf984a2cf5795207dbb583f6d3f341d27142ce85e6b
x64mingw=ddd12f80c979a4fc2485ebf984a2cf5795207dbb583f6d3f341d27142ce85e6b
x64freebsd=2b30f6e86c89dac7a6370e2565ea132cb5a7228c11cff1503b72b8451cba2476
x64openbsd=d9638dba3b4d00be62685b9e9c6f33bcad3853d52f38c6ccd79d2d5a42fcefa3
x64netbsd=fefe200627ad399311b62e108c33bef88c9fdc9b4c93f7bebeeac13ebef58104
x64musl=fefe200627ad399311b62e108c33bef88c9fdc9b4c93f7bebeeac13ebef58104
x64glibc=fefe200627ad399311b62e108c33bef88c9fdc9b4c93f7bebeeac13ebef58104
x64linux=fefe200627ad399311b62e108c33bef88c9fdc9b4c93f7bebeeac13ebef58104
x64elf=fefe200627ad399311b62e108c33bef88c9fdc9b4c93f7bebeeac13ebef58104
x64v1mac=a005d8d084eef1ff1eb75ea9ca67d1dd0ad2b68c2ed387ec0eea41869450e9bd
x64v1win=ddd12f80c979a4fc2485ebf984a2cf5795207dbb583f6d3f341d27142ce85e6b
x64v1mingw=ddd12f80c979a4fc2485ebf984a2cf5795207dbb583f6d3f341d27142ce85e6b
x64v1freebsd=2b30f6e86c89dac7a6370e2565ea132cb5a7228c11cff1503b72b8451cba2476
x64v1openbsd=d9638dba3b4d00be62685b9e9c6f33bcad3853d52f38c6ccd79d2d5a42fcefa3
x64v1netbsd=fefe200627ad399311b62e108c33bef88c9fdc9b4c93f7bebeeac13ebef58104
x64v1musl=fefe200627ad399311b62e108c33bef88c9fdc9b4c93f7bebeeac13ebef58104
x64v1glibc=fefe200627ad399311b62e108c33bef88c9fdc9b4c93f7bebeeac13ebef58104
x64v1linux=fefe200627ad399311b62e108c33bef88c9fdc9b4c93f7bebeeac13ebef58104
x64v1elf=fefe200627ad399311b62e108c33bef88c9fdc9b4c93f7bebeeac13ebef58104
arm64mac=d8d53cc723e4087b885f1c57006c80816639eccb7ca0c6286059c6b9825839cc
arm64win=5408e92fe20ac621f3a529f40d8ee6b77e72570b5766d6e3d4a4e608b23fbd79
arm64mingw=5408e92fe20ac621f3a529f40d8ee6b77e72570b5766d6e3d4a4e608b23fbd79
arm64linux=62a04177afcb3250c237fd8df8e2c2e87aea25a019f3b0c0e55d812dfa115dcf
arm64musl=62a04177afcb3250c237fd8df8e2c2e87aea25a019f3b0c0e55d812dfa115dcf
arm64glibc=62a04177afcb3250c237fd8df8e2c2e87aea25a019f3b0c0e55d812dfa115dcf
arm64v1win=5408e92fe20ac621f3a529f40d8ee6b77e72570b5766d6e3d4a4e608b23fbd79
arm64v1mingw=5408e92fe20ac621f3a529f40d8ee6b77e72570b5766d6e3d4a4e608b23fbd79
arm64v1linux=62a04177afcb3250c237fd8df8e2c2e87aea25a019f3b0c0e55d812dfa115dcf
arm64v1musl=62a04177afcb3250c237fd8df8e2c2e87aea25a019f3b0c0e55d812dfa115dcf
arm64v1glibc=62a04177afcb3250c237fd8df8e2c2e87aea25a019f3b0c0e55d812dfa115dcf
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
