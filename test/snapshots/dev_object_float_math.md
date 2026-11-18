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
x64mac=2896c6357eb60d494fc4e51ceae567b511fff4dc4b5350661ae2fd5753068319
x64win=f2f6067de5245210339cb296a7f7ebecb5b62c28233d47541bf575d76cebe048
x64mingw=f2f6067de5245210339cb296a7f7ebecb5b62c28233d47541bf575d76cebe048
x64freebsd=70f2abb2b2a225bd75f359261045e85dae12bd49358e9731afc0ebddeda13e8d
x64openbsd=5178b8d4aa48855231c653f4587c510067584dbe6521508392523cf971faf852
x64netbsd=819d4c8db02453292ec77dad60cbdf584a6e6c7220e8a5823ee5a938ae7b5b1e
x64musl=819d4c8db02453292ec77dad60cbdf584a6e6c7220e8a5823ee5a938ae7b5b1e
x64glibc=819d4c8db02453292ec77dad60cbdf584a6e6c7220e8a5823ee5a938ae7b5b1e
x64linux=819d4c8db02453292ec77dad60cbdf584a6e6c7220e8a5823ee5a938ae7b5b1e
x64elf=819d4c8db02453292ec77dad60cbdf584a6e6c7220e8a5823ee5a938ae7b5b1e
x64v1mac=2896c6357eb60d494fc4e51ceae567b511fff4dc4b5350661ae2fd5753068319
x64v1win=f2f6067de5245210339cb296a7f7ebecb5b62c28233d47541bf575d76cebe048
x64v1mingw=f2f6067de5245210339cb296a7f7ebecb5b62c28233d47541bf575d76cebe048
x64v1freebsd=70f2abb2b2a225bd75f359261045e85dae12bd49358e9731afc0ebddeda13e8d
x64v1openbsd=5178b8d4aa48855231c653f4587c510067584dbe6521508392523cf971faf852
x64v1netbsd=819d4c8db02453292ec77dad60cbdf584a6e6c7220e8a5823ee5a938ae7b5b1e
x64v1musl=819d4c8db02453292ec77dad60cbdf584a6e6c7220e8a5823ee5a938ae7b5b1e
x64v1glibc=819d4c8db02453292ec77dad60cbdf584a6e6c7220e8a5823ee5a938ae7b5b1e
x64v1linux=819d4c8db02453292ec77dad60cbdf584a6e6c7220e8a5823ee5a938ae7b5b1e
x64v1elf=819d4c8db02453292ec77dad60cbdf584a6e6c7220e8a5823ee5a938ae7b5b1e
arm64mac=d23e3b9d90f2a748236a8dd4432039f62dc7bc9fcf50640bf295cc78086c7f9d
arm64win=2212afbdc9c59364104fbe5bb1b97a2c1fe4cbdd964afcf9778d1c776d217ac7
arm64mingw=2212afbdc9c59364104fbe5bb1b97a2c1fe4cbdd964afcf9778d1c776d217ac7
arm64linux=e97637f9a3b4484dfb39f3213b3e45120510f8516b1d8fcc20ed584a61326606
arm64musl=e97637f9a3b4484dfb39f3213b3e45120510f8516b1d8fcc20ed584a61326606
arm64glibc=e97637f9a3b4484dfb39f3213b3e45120510f8516b1d8fcc20ed584a61326606
arm64v1win=2212afbdc9c59364104fbe5bb1b97a2c1fe4cbdd964afcf9778d1c776d217ac7
arm64v1mingw=2212afbdc9c59364104fbe5bb1b97a2c1fe4cbdd964afcf9778d1c776d217ac7
arm64v1linux=e97637f9a3b4484dfb39f3213b3e45120510f8516b1d8fcc20ed584a61326606
arm64v1musl=e97637f9a3b4484dfb39f3213b3e45120510f8516b1d8fcc20ed584a61326606
arm64v1glibc=e97637f9a3b4484dfb39f3213b3e45120510f8516b1d8fcc20ed584a61326606
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
