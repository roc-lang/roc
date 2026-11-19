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
x64mac=21339a9c654dc08e0b7f39474524f4afc13134aa2fc98d90e2bf94539128cdc3
x64win=2012e0effd91f7be39c36d55ac10e909356dc11cbe9d27bd9ff95c0e2375c927
x64mingw=2012e0effd91f7be39c36d55ac10e909356dc11cbe9d27bd9ff95c0e2375c927
x64freebsd=6a9a5752aa6a8e64a539c86215b4d6ceb93b28a38289a1d9786600de2b86aaf8
x64openbsd=36d0a98c094ecc382ca732725bb522787eb1eac4734c95cae97c4e7992096849
x64netbsd=1c2beadbf84a3eca88f8c980b17edd22af40d9a94b6dfbfdd58c1460005955df
x64musl=1c2beadbf84a3eca88f8c980b17edd22af40d9a94b6dfbfdd58c1460005955df
x64glibc=1c2beadbf84a3eca88f8c980b17edd22af40d9a94b6dfbfdd58c1460005955df
x64linux=1c2beadbf84a3eca88f8c980b17edd22af40d9a94b6dfbfdd58c1460005955df
x64elf=1c2beadbf84a3eca88f8c980b17edd22af40d9a94b6dfbfdd58c1460005955df
x64v1mac=21339a9c654dc08e0b7f39474524f4afc13134aa2fc98d90e2bf94539128cdc3
x64v1win=2012e0effd91f7be39c36d55ac10e909356dc11cbe9d27bd9ff95c0e2375c927
x64v1mingw=2012e0effd91f7be39c36d55ac10e909356dc11cbe9d27bd9ff95c0e2375c927
x64v1freebsd=6a9a5752aa6a8e64a539c86215b4d6ceb93b28a38289a1d9786600de2b86aaf8
x64v1openbsd=36d0a98c094ecc382ca732725bb522787eb1eac4734c95cae97c4e7992096849
x64v1netbsd=1c2beadbf84a3eca88f8c980b17edd22af40d9a94b6dfbfdd58c1460005955df
x64v1musl=1c2beadbf84a3eca88f8c980b17edd22af40d9a94b6dfbfdd58c1460005955df
x64v1glibc=1c2beadbf84a3eca88f8c980b17edd22af40d9a94b6dfbfdd58c1460005955df
x64v1linux=1c2beadbf84a3eca88f8c980b17edd22af40d9a94b6dfbfdd58c1460005955df
x64v1elf=1c2beadbf84a3eca88f8c980b17edd22af40d9a94b6dfbfdd58c1460005955df
arm64mac=6fd17d3dc4eb7a73da5f1d49f694b4835e6e7c68dd1dea526ebcb8182d048168
arm64win=2f5a87af87d7361576879ee896ac40d4cfcc5a04ba1e28b4d5009a3ab9c911e1
arm64mingw=2f5a87af87d7361576879ee896ac40d4cfcc5a04ba1e28b4d5009a3ab9c911e1
arm64linux=1571d472499a9f25710e6ea384b445f534338ab2ec93dedbc07c7a28871ee476
arm64musl=1571d472499a9f25710e6ea384b445f534338ab2ec93dedbc07c7a28871ee476
arm64glibc=1571d472499a9f25710e6ea384b445f534338ab2ec93dedbc07c7a28871ee476
arm64v1win=2f5a87af87d7361576879ee896ac40d4cfcc5a04ba1e28b4d5009a3ab9c911e1
arm64v1mingw=2f5a87af87d7361576879ee896ac40d4cfcc5a04ba1e28b4d5009a3ab9c911e1
arm64v1linux=1571d472499a9f25710e6ea384b445f534338ab2ec93dedbc07c7a28871ee476
arm64v1musl=1571d472499a9f25710e6ea384b445f534338ab2ec93dedbc07c7a28871ee476
arm64v1glibc=1571d472499a9f25710e6ea384b445f534338ab2ec93dedbc07c7a28871ee476
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
