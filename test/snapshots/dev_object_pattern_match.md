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
x64mac=6813a69a9cbc783658e0d69ed2a69c37807e27005286c69746e16137eb3841f1
x64win=8b83c25ae6c0f06f5a4b51fc3ae1f180823b6b4e9f35fbf42b1505c9abe81c59
x64freebsd=a0690339373a5b99f39ce6658552b441ff07c9705530009267c231b92748f4be
x64openbsd=a0690339373a5b99f39ce6658552b441ff07c9705530009267c231b92748f4be
x64netbsd=a0690339373a5b99f39ce6658552b441ff07c9705530009267c231b92748f4be
x64musl=a0690339373a5b99f39ce6658552b441ff07c9705530009267c231b92748f4be
x64glibc=a0690339373a5b99f39ce6658552b441ff07c9705530009267c231b92748f4be
x64linux=a0690339373a5b99f39ce6658552b441ff07c9705530009267c231b92748f4be
x64elf=a0690339373a5b99f39ce6658552b441ff07c9705530009267c231b92748f4be
arm64mac=0efd4417c2ac5251a8eee9c260b891b3ff8c1bde5ffdd2781357e8c412f3a09a
arm64win=4ace3f84e7785c77feae5da711b316e77b2424e67e7bb3a9ab66382f4a320d02
arm64linux=203c2c903c050868a8e882f5a8b4da39b6db811ecac5026cd2bbc48ca4c8faee
arm64musl=203c2c903c050868a8e882f5a8b4da39b6db811ecac5026cd2bbc48ca4c8faee
arm64glibc=203c2c903c050868a8e882f5a8b4da39b6db811ecac5026cd2bbc48ca4c8faee
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
