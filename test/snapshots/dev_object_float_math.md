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
x64mac=112514288e2fded5c0e752939c7b1746b6d0a75c92cca8fd62a3ebf3669d0107
x64win=d3fd6ca1a1bd69df1fbe699b0029b14e16643e2cffc1c47fae0a5aeba88cbc79
x64mingw=d3fd6ca1a1bd69df1fbe699b0029b14e16643e2cffc1c47fae0a5aeba88cbc79
x64freebsd=2ed068604dd211cba64356c585c83ebc942ec2362bd890a4f74a504e20c5a02d
x64openbsd=c44f9435b99f6b5005e8564f38f52eaf822f9b177192120f664a566e7ddaee7a
x64netbsd=efbe9715ceb0b8e38f8c1fa049622896dd2b96c303fb79755e2b333b01b98ce4
x64musl=efbe9715ceb0b8e38f8c1fa049622896dd2b96c303fb79755e2b333b01b98ce4
x64glibc=efbe9715ceb0b8e38f8c1fa049622896dd2b96c303fb79755e2b333b01b98ce4
x64linux=efbe9715ceb0b8e38f8c1fa049622896dd2b96c303fb79755e2b333b01b98ce4
x64elf=efbe9715ceb0b8e38f8c1fa049622896dd2b96c303fb79755e2b333b01b98ce4
x64v1mac=112514288e2fded5c0e752939c7b1746b6d0a75c92cca8fd62a3ebf3669d0107
x64v1win=d3fd6ca1a1bd69df1fbe699b0029b14e16643e2cffc1c47fae0a5aeba88cbc79
x64v1mingw=d3fd6ca1a1bd69df1fbe699b0029b14e16643e2cffc1c47fae0a5aeba88cbc79
x64v1freebsd=2ed068604dd211cba64356c585c83ebc942ec2362bd890a4f74a504e20c5a02d
x64v1openbsd=c44f9435b99f6b5005e8564f38f52eaf822f9b177192120f664a566e7ddaee7a
x64v1netbsd=efbe9715ceb0b8e38f8c1fa049622896dd2b96c303fb79755e2b333b01b98ce4
x64v1musl=efbe9715ceb0b8e38f8c1fa049622896dd2b96c303fb79755e2b333b01b98ce4
x64v1glibc=efbe9715ceb0b8e38f8c1fa049622896dd2b96c303fb79755e2b333b01b98ce4
x64v1linux=efbe9715ceb0b8e38f8c1fa049622896dd2b96c303fb79755e2b333b01b98ce4
x64v1elf=efbe9715ceb0b8e38f8c1fa049622896dd2b96c303fb79755e2b333b01b98ce4
arm64mac=bba25c0505b2a244eab4955a7e41e1025a76e91cb024a459377e037343854c3e
arm64win=0e2e541935c34fd2727e5a3072211a0644ae0ee21adb4a0bdff02682da7439bc
arm64mingw=0e2e541935c34fd2727e5a3072211a0644ae0ee21adb4a0bdff02682da7439bc
arm64linux=1f651f95d03c6da5323508b7be610ba5f98c96decb8056ecd9198c2c32f96641
arm64musl=1f651f95d03c6da5323508b7be610ba5f98c96decb8056ecd9198c2c32f96641
arm64glibc=1f651f95d03c6da5323508b7be610ba5f98c96decb8056ecd9198c2c32f96641
arm64v1win=0e2e541935c34fd2727e5a3072211a0644ae0ee21adb4a0bdff02682da7439bc
arm64v1mingw=0e2e541935c34fd2727e5a3072211a0644ae0ee21adb4a0bdff02682da7439bc
arm64v1linux=1f651f95d03c6da5323508b7be610ba5f98c96decb8056ecd9198c2c32f96641
arm64v1musl=1f651f95d03c6da5323508b7be610ba5f98c96decb8056ecd9198c2c32f96641
arm64v1glibc=1f651f95d03c6da5323508b7be610ba5f98c96decb8056ecd9198c2c32f96641
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
