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
x64mac=e4ea979e006a770c9aa34280e16cef9e7c14c380e75f1306ff266d395044ade2
x64win=940bb59a2cd7580156ef56a24d5ef3c74d324e5056e6039b79508640fe3d7730
x64mingw=940bb59a2cd7580156ef56a24d5ef3c74d324e5056e6039b79508640fe3d7730
x64freebsd=6af3a3c14fd8782c9b9f619cdb710d7bec238a80f00ebc87b83c19b4601ada3e
x64openbsd=c4d48256c2c11b89180ee0c66d21de245614dc575789a99d85aa9d0e817a460d
x64netbsd=0cebd8a1f2c122a33c79b5a960ec9b73bcdbe29ec2ad5f597eb4f934604760f6
x64musl=0cebd8a1f2c122a33c79b5a960ec9b73bcdbe29ec2ad5f597eb4f934604760f6
x64glibc=0cebd8a1f2c122a33c79b5a960ec9b73bcdbe29ec2ad5f597eb4f934604760f6
x64linux=0cebd8a1f2c122a33c79b5a960ec9b73bcdbe29ec2ad5f597eb4f934604760f6
x64elf=0cebd8a1f2c122a33c79b5a960ec9b73bcdbe29ec2ad5f597eb4f934604760f6
x64v1mac=e4ea979e006a770c9aa34280e16cef9e7c14c380e75f1306ff266d395044ade2
x64v1win=940bb59a2cd7580156ef56a24d5ef3c74d324e5056e6039b79508640fe3d7730
x64v1mingw=940bb59a2cd7580156ef56a24d5ef3c74d324e5056e6039b79508640fe3d7730
x64v1freebsd=6af3a3c14fd8782c9b9f619cdb710d7bec238a80f00ebc87b83c19b4601ada3e
x64v1openbsd=c4d48256c2c11b89180ee0c66d21de245614dc575789a99d85aa9d0e817a460d
x64v1netbsd=0cebd8a1f2c122a33c79b5a960ec9b73bcdbe29ec2ad5f597eb4f934604760f6
x64v1musl=0cebd8a1f2c122a33c79b5a960ec9b73bcdbe29ec2ad5f597eb4f934604760f6
x64v1glibc=0cebd8a1f2c122a33c79b5a960ec9b73bcdbe29ec2ad5f597eb4f934604760f6
x64v1linux=0cebd8a1f2c122a33c79b5a960ec9b73bcdbe29ec2ad5f597eb4f934604760f6
x64v1elf=0cebd8a1f2c122a33c79b5a960ec9b73bcdbe29ec2ad5f597eb4f934604760f6
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
