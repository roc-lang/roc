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
x64mac=63c0bb192639dcdbbb740b33b739ebf7f3054dacd3c60636f7bc86d026404135
x64win=f12a9004ececdc0380c02c39a5236644c4355deed262f7f069825449f62e42e0
x64freebsd=553b2c29d1ee74dfb153e7a4840342ee23da0ac7254bf9b2c8d3f58c3320e97f
x64openbsd=553b2c29d1ee74dfb153e7a4840342ee23da0ac7254bf9b2c8d3f58c3320e97f
x64netbsd=553b2c29d1ee74dfb153e7a4840342ee23da0ac7254bf9b2c8d3f58c3320e97f
x64musl=553b2c29d1ee74dfb153e7a4840342ee23da0ac7254bf9b2c8d3f58c3320e97f
x64glibc=553b2c29d1ee74dfb153e7a4840342ee23da0ac7254bf9b2c8d3f58c3320e97f
x64linux=553b2c29d1ee74dfb153e7a4840342ee23da0ac7254bf9b2c8d3f58c3320e97f
x64elf=553b2c29d1ee74dfb153e7a4840342ee23da0ac7254bf9b2c8d3f58c3320e97f
arm64mac=2bafc3601de68d45bb3a9fa360acbdd79c2930c94b8d1d2e539456ce887431a8
arm64win=22ae0a18b51119d99cb26c8fe20d0fb8640ee60cc287ddc678c3a65bd64ad09e
arm64linux=09cad3998196ad8da7489c2c0e307b75159400e2d14ed9ca935fec5bc19a9ce1
arm64musl=09cad3998196ad8da7489c2c0e307b75159400e2d14ed9ca935fec5bc19a9ce1
arm64glibc=09cad3998196ad8da7489c2c0e307b75159400e2d14ed9ca935fec5bc19a9ce1
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
