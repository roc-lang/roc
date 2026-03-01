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
x64mac=a2b56d7f86256d16c7d7ee76f720677b8d1d29849cb77b4c9acfb86a7b7cc956
x64win=750b8d8c866a50bf83fa2054de047b1f819fc617178bdbc45fbe862f313422f9
x64freebsd=70e81e80e0f49ed1f3efab58166ca63f0f2729f474532b8c2202194870d3572c
x64openbsd=70e81e80e0f49ed1f3efab58166ca63f0f2729f474532b8c2202194870d3572c
x64netbsd=70e81e80e0f49ed1f3efab58166ca63f0f2729f474532b8c2202194870d3572c
x64musl=70e81e80e0f49ed1f3efab58166ca63f0f2729f474532b8c2202194870d3572c
x64glibc=70e81e80e0f49ed1f3efab58166ca63f0f2729f474532b8c2202194870d3572c
x64linux=70e81e80e0f49ed1f3efab58166ca63f0f2729f474532b8c2202194870d3572c
x64elf=70e81e80e0f49ed1f3efab58166ca63f0f2729f474532b8c2202194870d3572c
arm64mac=575be53fabc4f36099ca7fe2f21b28a4bd65decbfb68cb5c09d2069bd0bc532e
arm64win=c773936ab9f002376c7f2dec14eb52e434bb5a0bd1707b0bf2df82f8b3d85e82
arm64linux=141703cb1f81a8abb30243ed1adaf0c85ea9567b0b212c877612064626d206f0
arm64musl=141703cb1f81a8abb30243ed1adaf0c85ea9567b0b212c877612064626d206f0
arm64glibc=141703cb1f81a8abb30243ed1adaf0c85ea9567b0b212c877612064626d206f0
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
