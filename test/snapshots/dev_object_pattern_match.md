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
x64mac=49885b0a6acb643cf39adbc9d159f5c67dc0eaefdb3d8984295be6282a8da7b9
x64win=62397c667d984ae0edcbd172b2c17560d30064947e14f4d2e287bc1803fa7bb6
x64freebsd=49c0d6ea4ee46dccd88b4888edc656ccbd1434e830eb62973fe62f716eec09ff
x64openbsd=49c0d6ea4ee46dccd88b4888edc656ccbd1434e830eb62973fe62f716eec09ff
x64netbsd=49c0d6ea4ee46dccd88b4888edc656ccbd1434e830eb62973fe62f716eec09ff
x64musl=49c0d6ea4ee46dccd88b4888edc656ccbd1434e830eb62973fe62f716eec09ff
x64glibc=49c0d6ea4ee46dccd88b4888edc656ccbd1434e830eb62973fe62f716eec09ff
x64linux=49c0d6ea4ee46dccd88b4888edc656ccbd1434e830eb62973fe62f716eec09ff
x64elf=49c0d6ea4ee46dccd88b4888edc656ccbd1434e830eb62973fe62f716eec09ff
arm64mac=fe91abf28437d0dce5bf8ba0b79aa0a8aa8f90a0cbf69660b7cf8995eb733192
arm64win=eb2c4646a3c044625208b62da17138c825abc42f6d054a64943264004b5f9812
arm64linux=6ad7389726f43a795beb9369a511829476df132cc856cfb3d2c543e03ec56af7
arm64musl=6ad7389726f43a795beb9369a511829476df132cc856cfb3d2c543e03ec56af7
arm64glibc=6ad7389726f43a795beb9369a511829476df132cc856cfb3d2c543e03ec56af7
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
