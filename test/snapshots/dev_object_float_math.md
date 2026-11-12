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
x64mac=6f4260ea0aafec0ee4759c9476589a63bc66f13179f50ed6261b4d6a78ad4571
x64win=cdb289e0f98e10c6b1988083c32bda908ca88178b7dab5d61e39dd1c1839abc3
x64mingw=cdb289e0f98e10c6b1988083c32bda908ca88178b7dab5d61e39dd1c1839abc3
x64freebsd=5f6bd07e9c6814d67bc18f9d8bfdf29dbb751e54b1616c8f23beb40de3125119
x64openbsd=18b838c6146db23656dff21faea24f93c4f86e182c48ee6c21a9f207382a43d9
x64netbsd=47257cf17e97cdc6718d10569616b084c9323b33822eb117f554a078ffe75073
x64musl=47257cf17e97cdc6718d10569616b084c9323b33822eb117f554a078ffe75073
x64glibc=47257cf17e97cdc6718d10569616b084c9323b33822eb117f554a078ffe75073
x64linux=47257cf17e97cdc6718d10569616b084c9323b33822eb117f554a078ffe75073
x64elf=47257cf17e97cdc6718d10569616b084c9323b33822eb117f554a078ffe75073
x64v1mac=6f4260ea0aafec0ee4759c9476589a63bc66f13179f50ed6261b4d6a78ad4571
x64v1win=cdb289e0f98e10c6b1988083c32bda908ca88178b7dab5d61e39dd1c1839abc3
x64v1mingw=cdb289e0f98e10c6b1988083c32bda908ca88178b7dab5d61e39dd1c1839abc3
x64v1freebsd=5f6bd07e9c6814d67bc18f9d8bfdf29dbb751e54b1616c8f23beb40de3125119
x64v1openbsd=18b838c6146db23656dff21faea24f93c4f86e182c48ee6c21a9f207382a43d9
x64v1netbsd=47257cf17e97cdc6718d10569616b084c9323b33822eb117f554a078ffe75073
x64v1musl=47257cf17e97cdc6718d10569616b084c9323b33822eb117f554a078ffe75073
x64v1glibc=47257cf17e97cdc6718d10569616b084c9323b33822eb117f554a078ffe75073
x64v1linux=47257cf17e97cdc6718d10569616b084c9323b33822eb117f554a078ffe75073
x64v1elf=47257cf17e97cdc6718d10569616b084c9323b33822eb117f554a078ffe75073
arm64mac=8c83e3c67dc4fb1d05bffe586e865cab4ea4fc0c4730a7b74e4a0400d46e813c
arm64win=e3d38900fc435652a5a668759b82b8ba54256bef4538078d233258fc56badb59
arm64mingw=e3d38900fc435652a5a668759b82b8ba54256bef4538078d233258fc56badb59
arm64linux=995b6854124c4c2ca43fed2c60b6f660b3e1bdbf97fbf73e0065fae538146c9b
arm64musl=995b6854124c4c2ca43fed2c60b6f660b3e1bdbf97fbf73e0065fae538146c9b
arm64glibc=995b6854124c4c2ca43fed2c60b6f660b3e1bdbf97fbf73e0065fae538146c9b
arm64v1win=e3d38900fc435652a5a668759b82b8ba54256bef4538078d233258fc56badb59
arm64v1mingw=e3d38900fc435652a5a668759b82b8ba54256bef4538078d233258fc56badb59
arm64v1linux=995b6854124c4c2ca43fed2c60b6f660b3e1bdbf97fbf73e0065fae538146c9b
arm64v1musl=995b6854124c4c2ca43fed2c60b6f660b3e1bdbf97fbf73e0065fae538146c9b
arm64v1glibc=995b6854124c4c2ca43fed2c60b6f660b3e1bdbf97fbf73e0065fae538146c9b
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
