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
x64mac=c3c39fa7145c015bebc23fb46abadbce86d1c8aff15c6e300cf6f6c2c31f838c
x64win=6298881eaea15076f3c7ede30fcaf2119f1ec11527bbb7c5a395c51999a16b5a
x64freebsd=9887817f8b030af9ba8849b0513ad0d7dbb44528d289693f2ac0a933b8df5b48
x64openbsd=9887817f8b030af9ba8849b0513ad0d7dbb44528d289693f2ac0a933b8df5b48
x64netbsd=9887817f8b030af9ba8849b0513ad0d7dbb44528d289693f2ac0a933b8df5b48
x64musl=9887817f8b030af9ba8849b0513ad0d7dbb44528d289693f2ac0a933b8df5b48
x64glibc=9887817f8b030af9ba8849b0513ad0d7dbb44528d289693f2ac0a933b8df5b48
x64linux=9887817f8b030af9ba8849b0513ad0d7dbb44528d289693f2ac0a933b8df5b48
x64elf=9887817f8b030af9ba8849b0513ad0d7dbb44528d289693f2ac0a933b8df5b48
arm64mac=c3596df0af32405c0e1f6fad072bc6f679dbf3b9c63234c1bc942e2a1beaf657
arm64win=b889128dd2634ab4eddc0a99595235e0125492febb190f17a3fa87ba26496b60
arm64linux=bb9dd78337dc54a3df00feee402640cf9cee4b038d709fb469f39ffe2da940d3
arm64musl=bb9dd78337dc54a3df00feee402640cf9cee4b038d709fb469f39ffe2da940d3
arm64glibc=bb9dd78337dc54a3df00feee402640cf9cee4b038d709fb469f39ffe2da940d3
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
