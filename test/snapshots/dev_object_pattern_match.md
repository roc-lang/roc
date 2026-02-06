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
x64mac=57f046f71a265077d0162563200b7b4e1532668638af3ba69326d253b3213c83
x64win=7427f3c1dff31222ab120d367203968aebcc3a1226f169cd0ed852506af60ff9
x64freebsd=97ead80eff77ab56ac74a8d07896734a5d4c20d5638b64b8ab1adf6db5afe06b
x64openbsd=97ead80eff77ab56ac74a8d07896734a5d4c20d5638b64b8ab1adf6db5afe06b
x64netbsd=97ead80eff77ab56ac74a8d07896734a5d4c20d5638b64b8ab1adf6db5afe06b
x64musl=97ead80eff77ab56ac74a8d07896734a5d4c20d5638b64b8ab1adf6db5afe06b
x64glibc=97ead80eff77ab56ac74a8d07896734a5d4c20d5638b64b8ab1adf6db5afe06b
x64linux=97ead80eff77ab56ac74a8d07896734a5d4c20d5638b64b8ab1adf6db5afe06b
x64elf=97ead80eff77ab56ac74a8d07896734a5d4c20d5638b64b8ab1adf6db5afe06b
arm64mac=fc05a24c224252cc7e388396152aae09561443216f7f83dc9399b39591652886
arm64win=92cb485ac2efef02635fe94695a3bab2bcf9da511616b2c2e02491d0264a30b6
arm64linux=6dcdd1e025a6e91c167a326152f9a73372885685616f75f0b353e35fbd5f2d3d
arm64musl=6dcdd1e025a6e91c167a326152f9a73372885685616f75f0b353e35fbd5f2d3d
arm64glibc=6dcdd1e025a6e91c167a326152f9a73372885685616f75f0b353e35fbd5f2d3d
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
