# META
~~~ini
description=Tag unions and pattern matching
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

Color : [Red, Green, Blue]

to_str : Color -> Str
to_str = |color|
    match color {
        Red => "red"
        Green => "green"
        Blue => "blue"
    }

main = to_str(Red)
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : Str }
    exposes []
    packages {}
    provides { main_for_host: "main" }
    targets: {
        files: "targets/",
        exe: {
            x64glibc: [app],
        }
    }

main_for_host : Str
main_for_host = main
~~~
# MONO
~~~roc
# app
to_str = |color| match color {
	Red => "red"
	Green => "green"
	Blue => "blue"
}
main = to_str(Red)

# platform
main_for_host = <required>

~~~
# DEV OUTPUT
~~~ini
x64mac=ed48466cf4431b34e278cd7031407a0730d8fd021e20e29352337fce2a614454
x64win=81607c76271e911ee6e19bd1a1c4c3e1b7c181bd70fe01756412351542614406
x64freebsd=677743111a35a3c213a9eb6834e1219ee3f8a8b39d19bdc6ecedd1df770287ba
x64openbsd=677743111a35a3c213a9eb6834e1219ee3f8a8b39d19bdc6ecedd1df770287ba
x64netbsd=677743111a35a3c213a9eb6834e1219ee3f8a8b39d19bdc6ecedd1df770287ba
x64musl=677743111a35a3c213a9eb6834e1219ee3f8a8b39d19bdc6ecedd1df770287ba
x64glibc=677743111a35a3c213a9eb6834e1219ee3f8a8b39d19bdc6ecedd1df770287ba
x64linux=677743111a35a3c213a9eb6834e1219ee3f8a8b39d19bdc6ecedd1df770287ba
x64elf=677743111a35a3c213a9eb6834e1219ee3f8a8b39d19bdc6ecedd1df770287ba
arm64mac=2bb8098af7dfad880fb9086dde15e3b84d7169664802c59a96893b4fb2ea0f37
arm64win=dab833269d1cf5b3481e740e1236424416a701814cd821da5f822610cf90b108
arm64linux=c7db53c0a9030c0634a484a3af68e2a79130241030f1f0d6e0e2ff5128f57ac8
arm64musl=c7db53c0a9030c0634a484a3af68e2a79130241030f1f0d6e0e2ff5128f57ac8
arm64glibc=c7db53c0a9030c0634a484a3af68e2a79130241030f1f0d6e0e2ff5128f57ac8
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
