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
x64mac=2d635c933356581df77bb9918ebca7ee7f71341bc73790b28a39dd68e49c244b
x64win=ed11b1c721a133f60c32f5b8375fb3d4797fb74805f157a4e23b99d6aa3a3106
x64freebsd=788ef149176d3df60a18675f1409ff145835c9498a28b186ca8aad67f487a3d2
x64openbsd=788ef149176d3df60a18675f1409ff145835c9498a28b186ca8aad67f487a3d2
x64netbsd=788ef149176d3df60a18675f1409ff145835c9498a28b186ca8aad67f487a3d2
x64musl=788ef149176d3df60a18675f1409ff145835c9498a28b186ca8aad67f487a3d2
x64glibc=788ef149176d3df60a18675f1409ff145835c9498a28b186ca8aad67f487a3d2
x64linux=788ef149176d3df60a18675f1409ff145835c9498a28b186ca8aad67f487a3d2
x64elf=788ef149176d3df60a18675f1409ff145835c9498a28b186ca8aad67f487a3d2
arm64mac=fc05a24c224252cc7e388396152aae09561443216f7f83dc9399b39591652886
arm64win=92cb485ac2efef02635fe94695a3bab2bcf9da511616b2c2e02491d0264a30b6
arm64linux=6dcdd1e025a6e91c167a326152f9a73372885685616f75f0b353e35fbd5f2d3d
arm64musl=6dcdd1e025a6e91c167a326152f9a73372885685616f75f0b353e35fbd5f2d3d
arm64glibc=6dcdd1e025a6e91c167a326152f9a73372885685616f75f0b353e35fbd5f2d3d
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
