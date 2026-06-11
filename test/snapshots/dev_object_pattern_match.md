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
    provides { "roc_main": main_for_host }
    targets: {
        inputs: "targets/",
        x64glibc: { inputs: [app] },
    }

main_for_host : Str
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# app
to_str = |color| match color {
	Red => "red"
	Green => "green"
	Blue => "blue"
}
main = to_str(Red)

~~~
# DEV OUTPUT
~~~ini
x64mac=773517ba730b2ae281f4e55f0019efcb8143f94c74f956b25afee3882bd0633c
x64win=828583c59228a6062a44f2feb6d9e042fbfb728e04b548aba5f23c0866092bcb
x64freebsd=7ec0c37040e09677b3d5f0eebac8a397a398b45a7c7c44591cd15b489fe74ef0
x64openbsd=7ec0c37040e09677b3d5f0eebac8a397a398b45a7c7c44591cd15b489fe74ef0
x64netbsd=7ec0c37040e09677b3d5f0eebac8a397a398b45a7c7c44591cd15b489fe74ef0
x64musl=7ec0c37040e09677b3d5f0eebac8a397a398b45a7c7c44591cd15b489fe74ef0
x64glibc=7ec0c37040e09677b3d5f0eebac8a397a398b45a7c7c44591cd15b489fe74ef0
x64linux=7ec0c37040e09677b3d5f0eebac8a397a398b45a7c7c44591cd15b489fe74ef0
x64elf=7ec0c37040e09677b3d5f0eebac8a397a398b45a7c7c44591cd15b489fe74ef0
arm64mac=352ebc08e0ee906b4bc9ed25ad55c0d9f3f010dcb590f8feb533c6d8e39e3517
arm64win=008c53d141d34720afdd3eb7edf190fd05b021584945bde81c957235de1faf13
arm64linux=ec435cdcf1f72d992a2d6567d4cab36fb7ebf68ce7e79183a63bdd72964345b4
arm64musl=ec435cdcf1f72d992a2d6567d4cab36fb7ebf68ce7e79183a63bdd72964345b4
arm64glibc=ec435cdcf1f72d992a2d6567d4cab36fb7ebf68ce7e79183a63bdd72964345b4
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
