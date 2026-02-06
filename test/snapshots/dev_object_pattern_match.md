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
x64mac=b93fa4f203f1b1944f3f18b16ff79b369cb92325f750c6497ff4d7c50b0d0951
x64win=1ca9d13b2f8da0ee032e1c5aa74f5e69a7c422dffa352e8b6ed50435172e68c9
x64freebsd=39e44e335b83f05781cf8a5aaf06f5031a04420da0ca42c6fb21714ec2360a70
x64openbsd=39e44e335b83f05781cf8a5aaf06f5031a04420da0ca42c6fb21714ec2360a70
x64netbsd=39e44e335b83f05781cf8a5aaf06f5031a04420da0ca42c6fb21714ec2360a70
x64musl=39e44e335b83f05781cf8a5aaf06f5031a04420da0ca42c6fb21714ec2360a70
x64glibc=39e44e335b83f05781cf8a5aaf06f5031a04420da0ca42c6fb21714ec2360a70
x64linux=39e44e335b83f05781cf8a5aaf06f5031a04420da0ca42c6fb21714ec2360a70
x64elf=39e44e335b83f05781cf8a5aaf06f5031a04420da0ca42c6fb21714ec2360a70
arm64mac=fc05a24c224252cc7e388396152aae09561443216f7f83dc9399b39591652886
arm64win=92cb485ac2efef02635fe94695a3bab2bcf9da511616b2c2e02491d0264a30b6
arm64linux=6dcdd1e025a6e91c167a326152f9a73372885685616f75f0b353e35fbd5f2d3d
arm64musl=6dcdd1e025a6e91c167a326152f9a73372885685616f75f0b353e35fbd5f2d3d
arm64glibc=6dcdd1e025a6e91c167a326152f9a73372885685616f75f0b353e35fbd5f2d3d
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
