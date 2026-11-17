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
x64mac=56330da0484fc5f995821869eeb495daaa11a701301b70f9be32c8eca6d59bff
x64win=da593044bdde8e1d8987c93c9a7588cb8baa1977f4d44f7361d0a898c5f08ecd
x64mingw=da593044bdde8e1d8987c93c9a7588cb8baa1977f4d44f7361d0a898c5f08ecd
x64freebsd=bc64b76abc525900bfe7fca842142d4b8479413ec566200b5893420bcbb10c06
x64openbsd=e7cfca0abc0eef500287bb7136f3c3af0ff7088b38688eb19b87ad7d80b1e458
x64netbsd=f1db07b29a2c635848e0c7255faf257805a0dca9340ab1717a3f02787e21c576
x64musl=f1db07b29a2c635848e0c7255faf257805a0dca9340ab1717a3f02787e21c576
x64glibc=f1db07b29a2c635848e0c7255faf257805a0dca9340ab1717a3f02787e21c576
x64linux=f1db07b29a2c635848e0c7255faf257805a0dca9340ab1717a3f02787e21c576
x64elf=f1db07b29a2c635848e0c7255faf257805a0dca9340ab1717a3f02787e21c576
x64v1mac=56330da0484fc5f995821869eeb495daaa11a701301b70f9be32c8eca6d59bff
x64v1win=da593044bdde8e1d8987c93c9a7588cb8baa1977f4d44f7361d0a898c5f08ecd
x64v1mingw=da593044bdde8e1d8987c93c9a7588cb8baa1977f4d44f7361d0a898c5f08ecd
x64v1freebsd=bc64b76abc525900bfe7fca842142d4b8479413ec566200b5893420bcbb10c06
x64v1openbsd=e7cfca0abc0eef500287bb7136f3c3af0ff7088b38688eb19b87ad7d80b1e458
x64v1netbsd=f1db07b29a2c635848e0c7255faf257805a0dca9340ab1717a3f02787e21c576
x64v1musl=f1db07b29a2c635848e0c7255faf257805a0dca9340ab1717a3f02787e21c576
x64v1glibc=f1db07b29a2c635848e0c7255faf257805a0dca9340ab1717a3f02787e21c576
x64v1linux=f1db07b29a2c635848e0c7255faf257805a0dca9340ab1717a3f02787e21c576
x64v1elf=f1db07b29a2c635848e0c7255faf257805a0dca9340ab1717a3f02787e21c576
arm64mac=f066eca1fa8bedf08dbecd2c5bf6690f54ab87cb6b44f53d8c9a884313b6d59c
arm64win=4ae0428276aead7ec5471bea520cc553d335a1a7e7f9523bcb025fd97cbc66a4
arm64mingw=4ae0428276aead7ec5471bea520cc553d335a1a7e7f9523bcb025fd97cbc66a4
arm64linux=89da12e08e91b72cf7d805f74978e370ddfc353272c74bae3d1819e5575894b6
arm64musl=89da12e08e91b72cf7d805f74978e370ddfc353272c74bae3d1819e5575894b6
arm64glibc=89da12e08e91b72cf7d805f74978e370ddfc353272c74bae3d1819e5575894b6
arm64v1win=4ae0428276aead7ec5471bea520cc553d335a1a7e7f9523bcb025fd97cbc66a4
arm64v1mingw=4ae0428276aead7ec5471bea520cc553d335a1a7e7f9523bcb025fd97cbc66a4
arm64v1linux=89da12e08e91b72cf7d805f74978e370ddfc353272c74bae3d1819e5575894b6
arm64v1musl=89da12e08e91b72cf7d805f74978e370ddfc353272c74bae3d1819e5575894b6
arm64v1glibc=89da12e08e91b72cf7d805f74978e370ddfc353272c74bae3d1819e5575894b6
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
