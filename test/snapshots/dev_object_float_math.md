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
x64mac=2974cd8b80f24d070b1ede8204dd7a9b4fa96ae638b1f38d342bfc4216e2b53f
x64win=38a39dbe92a9807073bcb2656d525973013c6efa3f3313b71dd39af257276693
x64mingw=38a39dbe92a9807073bcb2656d525973013c6efa3f3313b71dd39af257276693
x64freebsd=b163cfa13afe2e00b77ac9a89ddf9033ca6754addef8128d80cbef4e89637205
x64openbsd=f657d0ffa61cf2b97bd8b872bc481dfa9f9a5786afb0bf692903c1a3c53f1a20
x64netbsd=55752b4e3c2ce17f895709ee41c9a28c77ced478ef00de3cba1f2d4476b08f43
x64musl=55752b4e3c2ce17f895709ee41c9a28c77ced478ef00de3cba1f2d4476b08f43
x64glibc=55752b4e3c2ce17f895709ee41c9a28c77ced478ef00de3cba1f2d4476b08f43
x64linux=55752b4e3c2ce17f895709ee41c9a28c77ced478ef00de3cba1f2d4476b08f43
x64elf=55752b4e3c2ce17f895709ee41c9a28c77ced478ef00de3cba1f2d4476b08f43
x64v1mac=2974cd8b80f24d070b1ede8204dd7a9b4fa96ae638b1f38d342bfc4216e2b53f
x64v1win=38a39dbe92a9807073bcb2656d525973013c6efa3f3313b71dd39af257276693
x64v1mingw=38a39dbe92a9807073bcb2656d525973013c6efa3f3313b71dd39af257276693
x64v1freebsd=b163cfa13afe2e00b77ac9a89ddf9033ca6754addef8128d80cbef4e89637205
x64v1openbsd=f657d0ffa61cf2b97bd8b872bc481dfa9f9a5786afb0bf692903c1a3c53f1a20
x64v1netbsd=55752b4e3c2ce17f895709ee41c9a28c77ced478ef00de3cba1f2d4476b08f43
x64v1musl=55752b4e3c2ce17f895709ee41c9a28c77ced478ef00de3cba1f2d4476b08f43
x64v1glibc=55752b4e3c2ce17f895709ee41c9a28c77ced478ef00de3cba1f2d4476b08f43
x64v1linux=55752b4e3c2ce17f895709ee41c9a28c77ced478ef00de3cba1f2d4476b08f43
x64v1elf=55752b4e3c2ce17f895709ee41c9a28c77ced478ef00de3cba1f2d4476b08f43
arm64mac=2ba0801c28aae081411e76379f7e23a9c37dbfbf3a290aa0b71df10dd0b4762d
arm64win=3f167f7e302622215cdd09891a57dfe498aac576dc9ed8d0b7d5ed10b2c4005f
arm64mingw=3f167f7e302622215cdd09891a57dfe498aac576dc9ed8d0b7d5ed10b2c4005f
arm64linux=2c26e5e37d4250b70dc11c5878e3be2baa68a527be354f136b9062297c88162c
arm64musl=2c26e5e37d4250b70dc11c5878e3be2baa68a527be354f136b9062297c88162c
arm64glibc=2c26e5e37d4250b70dc11c5878e3be2baa68a527be354f136b9062297c88162c
arm64v1win=3f167f7e302622215cdd09891a57dfe498aac576dc9ed8d0b7d5ed10b2c4005f
arm64v1mingw=3f167f7e302622215cdd09891a57dfe498aac576dc9ed8d0b7d5ed10b2c4005f
arm64v1linux=2c26e5e37d4250b70dc11c5878e3be2baa68a527be354f136b9062297c88162c
arm64v1musl=2c26e5e37d4250b70dc11c5878e3be2baa68a527be354f136b9062297c88162c
arm64v1glibc=2c26e5e37d4250b70dc11c5878e3be2baa68a527be354f136b9062297c88162c
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
