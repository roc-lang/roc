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
x64mac=adf955129fd698101888125829cd15f4323dfbdfa1361d70cc7aab727cec0bd3
x64win=22debd8e7cbc00f599cc9695a52d040dabebfd66ec6dcfec285c0bcc9ff19447
x64freebsd=c87a34cbbd535b61bb06f3d3f0f3737e7d73432f36f7d9373dd1d41616b4a418
x64openbsd=c87a34cbbd535b61bb06f3d3f0f3737e7d73432f36f7d9373dd1d41616b4a418
x64netbsd=c87a34cbbd535b61bb06f3d3f0f3737e7d73432f36f7d9373dd1d41616b4a418
x64musl=c87a34cbbd535b61bb06f3d3f0f3737e7d73432f36f7d9373dd1d41616b4a418
x64glibc=c87a34cbbd535b61bb06f3d3f0f3737e7d73432f36f7d9373dd1d41616b4a418
x64linux=c87a34cbbd535b61bb06f3d3f0f3737e7d73432f36f7d9373dd1d41616b4a418
x64elf=c87a34cbbd535b61bb06f3d3f0f3737e7d73432f36f7d9373dd1d41616b4a418
arm64mac=e58379bf4437258ee1447b42709c2a10e9fb0ad476449e672060b05626dedf31
arm64win=8010fafe89fe30e6f4ab7500a72849a09e413cdc53c633f0e71d0abd69433aad
arm64linux=c051ca1d15c16c9cd6d247682cd281dbc300eb5e3dc742ba1feb64bac15115a1
arm64musl=c051ca1d15c16c9cd6d247682cd281dbc300eb5e3dc742ba1feb64bac15115a1
arm64glibc=c051ca1d15c16c9cd6d247682cd281dbc300eb5e3dc742ba1feb64bac15115a1
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
