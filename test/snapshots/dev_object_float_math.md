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
x64mac=6fd5136f3792850017b66446ecf13dc17e6e079b8f64c80892afd438973dac06
x64win=efe16a28d32e1f2b4cef3e899999ff76194a48ed8c0ec8f228b766dbe5475098
x64mingw=efe16a28d32e1f2b4cef3e899999ff76194a48ed8c0ec8f228b766dbe5475098
x64freebsd=115b22e345ac30f0236504b531fcec95eac7be53ceb4f3b472e6c4289ad7b6f9
x64openbsd=815ff1c8c357a7619d79d34e497e516c74c2bb7218c77e720a86b43062279440
x64netbsd=d25a35d7e048dd89e17d5ce6790b564fbe4b3e99133e08fca0a4f27f4bca07f1
x64musl=d25a35d7e048dd89e17d5ce6790b564fbe4b3e99133e08fca0a4f27f4bca07f1
x64glibc=d25a35d7e048dd89e17d5ce6790b564fbe4b3e99133e08fca0a4f27f4bca07f1
x64linux=d25a35d7e048dd89e17d5ce6790b564fbe4b3e99133e08fca0a4f27f4bca07f1
x64elf=d25a35d7e048dd89e17d5ce6790b564fbe4b3e99133e08fca0a4f27f4bca07f1
x64v1mac=6fd5136f3792850017b66446ecf13dc17e6e079b8f64c80892afd438973dac06
x64v1win=efe16a28d32e1f2b4cef3e899999ff76194a48ed8c0ec8f228b766dbe5475098
x64v1mingw=efe16a28d32e1f2b4cef3e899999ff76194a48ed8c0ec8f228b766dbe5475098
x64v1freebsd=115b22e345ac30f0236504b531fcec95eac7be53ceb4f3b472e6c4289ad7b6f9
x64v1openbsd=815ff1c8c357a7619d79d34e497e516c74c2bb7218c77e720a86b43062279440
x64v1netbsd=d25a35d7e048dd89e17d5ce6790b564fbe4b3e99133e08fca0a4f27f4bca07f1
x64v1musl=d25a35d7e048dd89e17d5ce6790b564fbe4b3e99133e08fca0a4f27f4bca07f1
x64v1glibc=d25a35d7e048dd89e17d5ce6790b564fbe4b3e99133e08fca0a4f27f4bca07f1
x64v1linux=d25a35d7e048dd89e17d5ce6790b564fbe4b3e99133e08fca0a4f27f4bca07f1
x64v1elf=d25a35d7e048dd89e17d5ce6790b564fbe4b3e99133e08fca0a4f27f4bca07f1
arm64mac=7390125e5b782fb22c05cbe2e2de0a8b8df85b8c10f6dfcb94968ab6ec94ba74
arm64win=1c4d0360a4a6122cebf7d34c3fabc34de2fd555393a90e0188ec33d4bfbf68c2
arm64mingw=1c4d0360a4a6122cebf7d34c3fabc34de2fd555393a90e0188ec33d4bfbf68c2
arm64linux=f0a0638e0cc6499f44ec49d0506e5d451a3578dcd4d5131e88aa5b2824a1144c
arm64musl=f0a0638e0cc6499f44ec49d0506e5d451a3578dcd4d5131e88aa5b2824a1144c
arm64glibc=f0a0638e0cc6499f44ec49d0506e5d451a3578dcd4d5131e88aa5b2824a1144c
arm64v1win=1c4d0360a4a6122cebf7d34c3fabc34de2fd555393a90e0188ec33d4bfbf68c2
arm64v1mingw=1c4d0360a4a6122cebf7d34c3fabc34de2fd555393a90e0188ec33d4bfbf68c2
arm64v1linux=f0a0638e0cc6499f44ec49d0506e5d451a3578dcd4d5131e88aa5b2824a1144c
arm64v1musl=f0a0638e0cc6499f44ec49d0506e5d451a3578dcd4d5131e88aa5b2824a1144c
arm64v1glibc=f0a0638e0cc6499f44ec49d0506e5d451a3578dcd4d5131e88aa5b2824a1144c
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
