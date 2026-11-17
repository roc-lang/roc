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
x64mac=84b97e35b2ae0826bb689b6eb198291407fbadca5211588782dd1911864c25d3
x64win=4830c632fa7653e33fe446e7cb0dc3b68de2d5029c7a7b362dd25bab624d7987
x64mingw=4830c632fa7653e33fe446e7cb0dc3b68de2d5029c7a7b362dd25bab624d7987
x64freebsd=80c089f50f8600b64c844d8495048719fe87390fdcdfdda19c5b78a0ad40005c
x64openbsd=15d06c5b008a5e8358c1442af6dff05872ca0e793405178cec60cca0de3a6dfa
x64netbsd=99ab1f31639d77ff1dd33539e4d010ed496f25d8e7deaa0364e47562fc11234d
x64musl=99ab1f31639d77ff1dd33539e4d010ed496f25d8e7deaa0364e47562fc11234d
x64glibc=99ab1f31639d77ff1dd33539e4d010ed496f25d8e7deaa0364e47562fc11234d
x64linux=99ab1f31639d77ff1dd33539e4d010ed496f25d8e7deaa0364e47562fc11234d
x64elf=99ab1f31639d77ff1dd33539e4d010ed496f25d8e7deaa0364e47562fc11234d
x64v1mac=84b97e35b2ae0826bb689b6eb198291407fbadca5211588782dd1911864c25d3
x64v1win=4830c632fa7653e33fe446e7cb0dc3b68de2d5029c7a7b362dd25bab624d7987
x64v1mingw=4830c632fa7653e33fe446e7cb0dc3b68de2d5029c7a7b362dd25bab624d7987
x64v1freebsd=80c089f50f8600b64c844d8495048719fe87390fdcdfdda19c5b78a0ad40005c
x64v1openbsd=15d06c5b008a5e8358c1442af6dff05872ca0e793405178cec60cca0de3a6dfa
x64v1netbsd=99ab1f31639d77ff1dd33539e4d010ed496f25d8e7deaa0364e47562fc11234d
x64v1musl=99ab1f31639d77ff1dd33539e4d010ed496f25d8e7deaa0364e47562fc11234d
x64v1glibc=99ab1f31639d77ff1dd33539e4d010ed496f25d8e7deaa0364e47562fc11234d
x64v1linux=99ab1f31639d77ff1dd33539e4d010ed496f25d8e7deaa0364e47562fc11234d
x64v1elf=99ab1f31639d77ff1dd33539e4d010ed496f25d8e7deaa0364e47562fc11234d
arm64mac=f16d7158186feda40e2f01cc81e2edaf0b5042bf64769b84c517baee89742bce
arm64win=26c93dedb0ebdb1baa4b22083fe64331350f6e0b5345660dc29e9d5e3322d983
arm64mingw=26c93dedb0ebdb1baa4b22083fe64331350f6e0b5345660dc29e9d5e3322d983
arm64linux=d966e1cb2ad9aae9940a87d52912922d6118c1ce034857882ec2e684d358f53e
arm64musl=d966e1cb2ad9aae9940a87d52912922d6118c1ce034857882ec2e684d358f53e
arm64glibc=d966e1cb2ad9aae9940a87d52912922d6118c1ce034857882ec2e684d358f53e
arm64v1win=26c93dedb0ebdb1baa4b22083fe64331350f6e0b5345660dc29e9d5e3322d983
arm64v1mingw=26c93dedb0ebdb1baa4b22083fe64331350f6e0b5345660dc29e9d5e3322d983
arm64v1linux=d966e1cb2ad9aae9940a87d52912922d6118c1ce034857882ec2e684d358f53e
arm64v1musl=d966e1cb2ad9aae9940a87d52912922d6118c1ce034857882ec2e684d358f53e
arm64v1glibc=d966e1cb2ad9aae9940a87d52912922d6118c1ce034857882ec2e684d358f53e
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
