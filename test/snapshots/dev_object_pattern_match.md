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
        inputs_dir: "targets/",
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
x64mac=f96145e297aa869f17c362a2bba5a81426a2042fae198290263c38ff9676b543
x64win=665a3ded04bd6402077a4fec2f38b94f916d94f3c303122d506dde40219a6422
x64mingw=665a3ded04bd6402077a4fec2f38b94f916d94f3c303122d506dde40219a6422
x64freebsd=1c21721a64819ec52b831ef324cbd57391248ad683a7dd329d5d4085f7ad941b
x64openbsd=eb2ffe1a5837881d7218f4152c98e65a5b095256c3143da96ad06ff00f69aeb4
x64netbsd=4134858f3ffe7440f6c43f065d3dbb93ac861d9279f65995fd0ea869a1b7b59e
x64musl=4134858f3ffe7440f6c43f065d3dbb93ac861d9279f65995fd0ea869a1b7b59e
x64glibc=4134858f3ffe7440f6c43f065d3dbb93ac861d9279f65995fd0ea869a1b7b59e
x64linux=4134858f3ffe7440f6c43f065d3dbb93ac861d9279f65995fd0ea869a1b7b59e
x64elf=4134858f3ffe7440f6c43f065d3dbb93ac861d9279f65995fd0ea869a1b7b59e
x64v1mac=f96145e297aa869f17c362a2bba5a81426a2042fae198290263c38ff9676b543
x64v1win=665a3ded04bd6402077a4fec2f38b94f916d94f3c303122d506dde40219a6422
x64v1mingw=665a3ded04bd6402077a4fec2f38b94f916d94f3c303122d506dde40219a6422
x64v1freebsd=1c21721a64819ec52b831ef324cbd57391248ad683a7dd329d5d4085f7ad941b
x64v1openbsd=eb2ffe1a5837881d7218f4152c98e65a5b095256c3143da96ad06ff00f69aeb4
x64v1netbsd=4134858f3ffe7440f6c43f065d3dbb93ac861d9279f65995fd0ea869a1b7b59e
x64v1musl=4134858f3ffe7440f6c43f065d3dbb93ac861d9279f65995fd0ea869a1b7b59e
x64v1glibc=4134858f3ffe7440f6c43f065d3dbb93ac861d9279f65995fd0ea869a1b7b59e
x64v1linux=4134858f3ffe7440f6c43f065d3dbb93ac861d9279f65995fd0ea869a1b7b59e
x64v1elf=4134858f3ffe7440f6c43f065d3dbb93ac861d9279f65995fd0ea869a1b7b59e
arm64mac=c897234d22319ed18c444991564f252367dc5c80f8f8e51f83bd1c7af9feec1c
arm64win=fa027ec89dbc676c2c140b0b41de0080a5cf79d25cde4f79cedb458687e5a3e3
arm64mingw=fa027ec89dbc676c2c140b0b41de0080a5cf79d25cde4f79cedb458687e5a3e3
arm64linux=212e167cc3c705611280c9beefd30513f38e938bb24ef6d642ef5f68f4fd1182
arm64musl=212e167cc3c705611280c9beefd30513f38e938bb24ef6d642ef5f68f4fd1182
arm64glibc=212e167cc3c705611280c9beefd30513f38e938bb24ef6d642ef5f68f4fd1182
arm64v1win=fa027ec89dbc676c2c140b0b41de0080a5cf79d25cde4f79cedb458687e5a3e3
arm64v1mingw=fa027ec89dbc676c2c140b0b41de0080a5cf79d25cde4f79cedb458687e5a3e3
arm64v1linux=212e167cc3c705611280c9beefd30513f38e938bb24ef6d642ef5f68f4fd1182
arm64v1musl=212e167cc3c705611280c9beefd30513f38e938bb24ef6d642ef5f68f4fd1182
arm64v1glibc=212e167cc3c705611280c9beefd30513f38e938bb24ef6d642ef5f68f4fd1182
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
