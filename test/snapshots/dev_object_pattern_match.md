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
arm64mac=2bb8098af7dfad880fb9086dde15e3b84d7169664802c59a96893b4fb2ea0f37
arm64win=dab833269d1cf5b3481e740e1236424416a701814cd821da5f822610cf90b108
arm64linux=c7db53c0a9030c0634a484a3af68e2a79130241030f1f0d6e0e2ff5128f57ac8
arm64musl=c7db53c0a9030c0634a484a3af68e2a79130241030f1f0d6e0e2ff5128f57ac8
arm64glibc=c7db53c0a9030c0634a484a3af68e2a79130241030f1f0d6e0e2ff5128f57ac8
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
