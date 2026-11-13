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
x64mac=5c44dc7f0c6419ad8461787939c8e7d0df0ec17f9f107c7c80551ebd7e98c0a0
x64win=c1abdfd8f8598fc13fbe6e446b5d5c4d057b158017c47c574a95b3a088b70226
x64mingw=c1abdfd8f8598fc13fbe6e446b5d5c4d057b158017c47c574a95b3a088b70226
x64freebsd=4100eaeb006a9331910d96f1a2625684b6d15307ac26069225abd4eeab066c26
x64openbsd=befcbb7cf6344b8fb99aa32d1720b2ba014ff1849fc92c84bc760579398a2b3f
x64netbsd=1e9cb31ecae4a1693585429a9ab9be583cb65e579c876a9d6cc450de26242c6d
x64musl=1e9cb31ecae4a1693585429a9ab9be583cb65e579c876a9d6cc450de26242c6d
x64glibc=1e9cb31ecae4a1693585429a9ab9be583cb65e579c876a9d6cc450de26242c6d
x64linux=1e9cb31ecae4a1693585429a9ab9be583cb65e579c876a9d6cc450de26242c6d
x64elf=1e9cb31ecae4a1693585429a9ab9be583cb65e579c876a9d6cc450de26242c6d
x64v1mac=5c44dc7f0c6419ad8461787939c8e7d0df0ec17f9f107c7c80551ebd7e98c0a0
x64v1win=c1abdfd8f8598fc13fbe6e446b5d5c4d057b158017c47c574a95b3a088b70226
x64v1mingw=c1abdfd8f8598fc13fbe6e446b5d5c4d057b158017c47c574a95b3a088b70226
x64v1freebsd=4100eaeb006a9331910d96f1a2625684b6d15307ac26069225abd4eeab066c26
x64v1openbsd=befcbb7cf6344b8fb99aa32d1720b2ba014ff1849fc92c84bc760579398a2b3f
x64v1netbsd=1e9cb31ecae4a1693585429a9ab9be583cb65e579c876a9d6cc450de26242c6d
x64v1musl=1e9cb31ecae4a1693585429a9ab9be583cb65e579c876a9d6cc450de26242c6d
x64v1glibc=1e9cb31ecae4a1693585429a9ab9be583cb65e579c876a9d6cc450de26242c6d
x64v1linux=1e9cb31ecae4a1693585429a9ab9be583cb65e579c876a9d6cc450de26242c6d
x64v1elf=1e9cb31ecae4a1693585429a9ab9be583cb65e579c876a9d6cc450de26242c6d
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
