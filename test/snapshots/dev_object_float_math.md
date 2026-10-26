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
x64mac=0a6a8023cf9e6481c40547844ef5868916c50647b9cf83e427d42c27bb1a78aa
x64win=e552bb9aed9b18bc9ff852cd054a12cacef51e0395ffb988d84526ad058eaeed
x64mingw=e552bb9aed9b18bc9ff852cd054a12cacef51e0395ffb988d84526ad058eaeed
x64freebsd=28b28ae5b26fc1d309236b75febd0a87744a83df9a72ccabb06c9c6c4fb775cd
x64openbsd=877ea46139cc449b654c88b549662efe8ce1c56ccb94b997ca5ae494603d7faa
x64netbsd=fa58d04888df47f4116c55a10efbacc5a1eb50113804dfbcd7ee3c71ec04b05f
x64musl=fa58d04888df47f4116c55a10efbacc5a1eb50113804dfbcd7ee3c71ec04b05f
x64glibc=fa58d04888df47f4116c55a10efbacc5a1eb50113804dfbcd7ee3c71ec04b05f
x64linux=fa58d04888df47f4116c55a10efbacc5a1eb50113804dfbcd7ee3c71ec04b05f
x64elf=fa58d04888df47f4116c55a10efbacc5a1eb50113804dfbcd7ee3c71ec04b05f
x64v1mac=0a6a8023cf9e6481c40547844ef5868916c50647b9cf83e427d42c27bb1a78aa
x64v1win=e552bb9aed9b18bc9ff852cd054a12cacef51e0395ffb988d84526ad058eaeed
x64v1mingw=e552bb9aed9b18bc9ff852cd054a12cacef51e0395ffb988d84526ad058eaeed
x64v1freebsd=28b28ae5b26fc1d309236b75febd0a87744a83df9a72ccabb06c9c6c4fb775cd
x64v1openbsd=877ea46139cc449b654c88b549662efe8ce1c56ccb94b997ca5ae494603d7faa
x64v1netbsd=fa58d04888df47f4116c55a10efbacc5a1eb50113804dfbcd7ee3c71ec04b05f
x64v1musl=fa58d04888df47f4116c55a10efbacc5a1eb50113804dfbcd7ee3c71ec04b05f
x64v1glibc=fa58d04888df47f4116c55a10efbacc5a1eb50113804dfbcd7ee3c71ec04b05f
x64v1linux=fa58d04888df47f4116c55a10efbacc5a1eb50113804dfbcd7ee3c71ec04b05f
x64v1elf=fa58d04888df47f4116c55a10efbacc5a1eb50113804dfbcd7ee3c71ec04b05f
arm64mac=4a3b95d88385254511c87faec3e78f9f5db70ab6586ad11caba8c604df09e9a7
arm64win=9466e5f20613a29d6aa31e9afb3a10996b008b1419a037c40154afaeae004173
arm64mingw=9466e5f20613a29d6aa31e9afb3a10996b008b1419a037c40154afaeae004173
arm64linux=201e3981c853167800866614f894c1325dbc9b584609388543b7914e95df5bd0
arm64musl=201e3981c853167800866614f894c1325dbc9b584609388543b7914e95df5bd0
arm64glibc=201e3981c853167800866614f894c1325dbc9b584609388543b7914e95df5bd0
arm64v1win=9466e5f20613a29d6aa31e9afb3a10996b008b1419a037c40154afaeae004173
arm64v1mingw=9466e5f20613a29d6aa31e9afb3a10996b008b1419a037c40154afaeae004173
arm64v1linux=201e3981c853167800866614f894c1325dbc9b584609388543b7914e95df5bd0
arm64v1musl=201e3981c853167800866614f894c1325dbc9b584609388543b7914e95df5bd0
arm64v1glibc=201e3981c853167800866614f894c1325dbc9b584609388543b7914e95df5bd0
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
