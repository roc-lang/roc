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
x64mac=cf802fffb0d41da5cfeae8427cde773209d3f338586d3c8f0270cf18e7c6657b
x64win=94fd5925c7afe498a9fffe9b16b0828ffbc4b6849ab38cd6b8554065d733f93c
x64mingw=94fd5925c7afe498a9fffe9b16b0828ffbc4b6849ab38cd6b8554065d733f93c
x64freebsd=b2dd8c020ac0562c03a5e885ea3a25674b9c7fe10b82298f74495dc7386b3f17
x64openbsd=0993e1f9fe1b1da1cfe46e8552fde6a5ad718d994e63ae735e89e4bb3a6d98d4
x64netbsd=8c2725ab7895dfcda283658f865d9b243b07b188fdb3cb44ccceafdeafb61db5
x64musl=8c2725ab7895dfcda283658f865d9b243b07b188fdb3cb44ccceafdeafb61db5
x64glibc=8c2725ab7895dfcda283658f865d9b243b07b188fdb3cb44ccceafdeafb61db5
x64linux=8c2725ab7895dfcda283658f865d9b243b07b188fdb3cb44ccceafdeafb61db5
x64elf=8c2725ab7895dfcda283658f865d9b243b07b188fdb3cb44ccceafdeafb61db5
x64v1mac=cf802fffb0d41da5cfeae8427cde773209d3f338586d3c8f0270cf18e7c6657b
x64v1win=94fd5925c7afe498a9fffe9b16b0828ffbc4b6849ab38cd6b8554065d733f93c
x64v1mingw=94fd5925c7afe498a9fffe9b16b0828ffbc4b6849ab38cd6b8554065d733f93c
x64v1freebsd=b2dd8c020ac0562c03a5e885ea3a25674b9c7fe10b82298f74495dc7386b3f17
x64v1openbsd=0993e1f9fe1b1da1cfe46e8552fde6a5ad718d994e63ae735e89e4bb3a6d98d4
x64v1netbsd=8c2725ab7895dfcda283658f865d9b243b07b188fdb3cb44ccceafdeafb61db5
x64v1musl=8c2725ab7895dfcda283658f865d9b243b07b188fdb3cb44ccceafdeafb61db5
x64v1glibc=8c2725ab7895dfcda283658f865d9b243b07b188fdb3cb44ccceafdeafb61db5
x64v1linux=8c2725ab7895dfcda283658f865d9b243b07b188fdb3cb44ccceafdeafb61db5
x64v1elf=8c2725ab7895dfcda283658f865d9b243b07b188fdb3cb44ccceafdeafb61db5
arm64mac=a7e23b5cc148af8b7a53d35203ee9a09c3d7ad9ecd177ab48de9568ac29801fc
arm64win=768244122ad86006100b740cf093ec48df1f08dc97c4b6543c5a7997002853c1
arm64mingw=768244122ad86006100b740cf093ec48df1f08dc97c4b6543c5a7997002853c1
arm64linux=27d1bc85d07244c1ee9e6765f2a7dd445c48190f320564dced6e5bf460a337fd
arm64musl=27d1bc85d07244c1ee9e6765f2a7dd445c48190f320564dced6e5bf460a337fd
arm64glibc=27d1bc85d07244c1ee9e6765f2a7dd445c48190f320564dced6e5bf460a337fd
arm64v1win=768244122ad86006100b740cf093ec48df1f08dc97c4b6543c5a7997002853c1
arm64v1mingw=768244122ad86006100b740cf093ec48df1f08dc97c4b6543c5a7997002853c1
arm64v1linux=27d1bc85d07244c1ee9e6765f2a7dd445c48190f320564dced6e5bf460a337fd
arm64v1musl=27d1bc85d07244c1ee9e6765f2a7dd445c48190f320564dced6e5bf460a337fd
arm64v1glibc=27d1bc85d07244c1ee9e6765f2a7dd445c48190f320564dced6e5bf460a337fd
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
