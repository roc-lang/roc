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
x64mac=a4f4737279229a7ebc661ba32222d0b46bb169d43559fee9e377faa564b3d05c
x64win=87835e64da72d4fa4d349a2820cd9d5f1406b58f8d61c59e95f525a38cb68d88
x64mingw=87835e64da72d4fa4d349a2820cd9d5f1406b58f8d61c59e95f525a38cb68d88
x64freebsd=faa31efe4638e5f9916fe1f0dbc48c7f60618b24398bb3eca13e2bbafe2af77e
x64openbsd=bcc102af6cd228e8bbf9cbc9dc75b117c49e4ecb2b82fe716c5268cb09ccd9cd
x64netbsd=faff9b9ef4b55e1539efdf87d26660b8e5b75b0b82b65c98d405762d43f7cd1f
x64musl=faff9b9ef4b55e1539efdf87d26660b8e5b75b0b82b65c98d405762d43f7cd1f
x64glibc=faff9b9ef4b55e1539efdf87d26660b8e5b75b0b82b65c98d405762d43f7cd1f
x64linux=faff9b9ef4b55e1539efdf87d26660b8e5b75b0b82b65c98d405762d43f7cd1f
x64elf=faff9b9ef4b55e1539efdf87d26660b8e5b75b0b82b65c98d405762d43f7cd1f
x64v1mac=a4f4737279229a7ebc661ba32222d0b46bb169d43559fee9e377faa564b3d05c
x64v1win=87835e64da72d4fa4d349a2820cd9d5f1406b58f8d61c59e95f525a38cb68d88
x64v1mingw=87835e64da72d4fa4d349a2820cd9d5f1406b58f8d61c59e95f525a38cb68d88
x64v1freebsd=faa31efe4638e5f9916fe1f0dbc48c7f60618b24398bb3eca13e2bbafe2af77e
x64v1openbsd=bcc102af6cd228e8bbf9cbc9dc75b117c49e4ecb2b82fe716c5268cb09ccd9cd
x64v1netbsd=faff9b9ef4b55e1539efdf87d26660b8e5b75b0b82b65c98d405762d43f7cd1f
x64v1musl=faff9b9ef4b55e1539efdf87d26660b8e5b75b0b82b65c98d405762d43f7cd1f
x64v1glibc=faff9b9ef4b55e1539efdf87d26660b8e5b75b0b82b65c98d405762d43f7cd1f
x64v1linux=faff9b9ef4b55e1539efdf87d26660b8e5b75b0b82b65c98d405762d43f7cd1f
x64v1elf=faff9b9ef4b55e1539efdf87d26660b8e5b75b0b82b65c98d405762d43f7cd1f
arm64mac=2afe119e1d92b8769434fdb25bc718790f7ff6fd80f3277ef0efbf454a0c9f34
arm64win=baf3eaf4675b9b347e40e491a4e49fea25af0a615e609555e9613ec4bb1122f9
arm64mingw=baf3eaf4675b9b347e40e491a4e49fea25af0a615e609555e9613ec4bb1122f9
arm64linux=d6ecbae468a17fd3c6e007f6f924a03bbd5491b9d9629df6260d0f4f27e19240
arm64musl=d6ecbae468a17fd3c6e007f6f924a03bbd5491b9d9629df6260d0f4f27e19240
arm64glibc=d6ecbae468a17fd3c6e007f6f924a03bbd5491b9d9629df6260d0f4f27e19240
arm64v1win=baf3eaf4675b9b347e40e491a4e49fea25af0a615e609555e9613ec4bb1122f9
arm64v1mingw=baf3eaf4675b9b347e40e491a4e49fea25af0a615e609555e9613ec4bb1122f9
arm64v1linux=d6ecbae468a17fd3c6e007f6f924a03bbd5491b9d9629df6260d0f4f27e19240
arm64v1musl=d6ecbae468a17fd3c6e007f6f924a03bbd5491b9d9629df6260d0f4f27e19240
arm64v1glibc=d6ecbae468a17fd3c6e007f6f924a03bbd5491b9d9629df6260d0f4f27e19240
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
