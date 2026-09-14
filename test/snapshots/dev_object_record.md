# META
~~~ini
description=Multiple provides entries with two entrypoints
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [name, score] { pf: platform "./platform.roc" }

name = "Alice"

score : I64
score = 42
~~~
## platform.roc
~~~roc
platform ""
    requires {} { name : Str, score : I64 }
    exposes []
    packages {}
    provides { "roc_name": name_for_host, "roc_score": score_for_host }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
    }

name_for_host : Str
name_for_host = name

score_for_host : I64
score_for_host = score
~~~
# MONO
~~~roc
# platform
name_for_host = <required>
score_for_host = <required>

# app
name = "Alice"
score = 42

~~~
# DEV OUTPUT
~~~ini
x64mac=baaeae346b74e008ea6186524cd50171ec6c75e405ab0d511e9e7a096bccac6a
x64win=f2c7edd530c3c811757c1a9c01d204b128f57deb07a417110d7004200119a6bd
x64mingw=f2c7edd530c3c811757c1a9c01d204b128f57deb07a417110d7004200119a6bd
x64freebsd=f5cd816ec1eee68898d65bb9c88019ee89952b4416fd2d9a812e6d60a60fd3a2
x64openbsd=4b0d2e89d4923b75c9bb8b1f6886856ddeea553c8cbaa09f97c2b46b2a871fcc
x64netbsd=7ee81c2cf82742b9e421273794fc2480a5ade195db3493614a93c7de22474a28
x64musl=7ee81c2cf82742b9e421273794fc2480a5ade195db3493614a93c7de22474a28
x64glibc=7ee81c2cf82742b9e421273794fc2480a5ade195db3493614a93c7de22474a28
x64linux=7ee81c2cf82742b9e421273794fc2480a5ade195db3493614a93c7de22474a28
x64elf=7ee81c2cf82742b9e421273794fc2480a5ade195db3493614a93c7de22474a28
x64v1mac=baaeae346b74e008ea6186524cd50171ec6c75e405ab0d511e9e7a096bccac6a
x64v1win=f2c7edd530c3c811757c1a9c01d204b128f57deb07a417110d7004200119a6bd
x64v1mingw=f2c7edd530c3c811757c1a9c01d204b128f57deb07a417110d7004200119a6bd
x64v1freebsd=f5cd816ec1eee68898d65bb9c88019ee89952b4416fd2d9a812e6d60a60fd3a2
x64v1openbsd=4b0d2e89d4923b75c9bb8b1f6886856ddeea553c8cbaa09f97c2b46b2a871fcc
x64v1netbsd=7ee81c2cf82742b9e421273794fc2480a5ade195db3493614a93c7de22474a28
x64v1musl=7ee81c2cf82742b9e421273794fc2480a5ade195db3493614a93c7de22474a28
x64v1glibc=7ee81c2cf82742b9e421273794fc2480a5ade195db3493614a93c7de22474a28
x64v1linux=7ee81c2cf82742b9e421273794fc2480a5ade195db3493614a93c7de22474a28
x64v1elf=7ee81c2cf82742b9e421273794fc2480a5ade195db3493614a93c7de22474a28
arm64mac=c19e498506133a2520c1ce97f6b627c89ea282392afebb646184e5f9ac4f409c
arm64win=0f85c53f41827ea6a52c00d7a614cf195553e3a842face3e8c64bbffd9b7da85
arm64mingw=0f85c53f41827ea6a52c00d7a614cf195553e3a842face3e8c64bbffd9b7da85
arm64linux=a78207f09d5592174719f1648d0aea7b39ee19590f4ef96ea661c12369ae4161
arm64musl=a78207f09d5592174719f1648d0aea7b39ee19590f4ef96ea661c12369ae4161
arm64glibc=a78207f09d5592174719f1648d0aea7b39ee19590f4ef96ea661c12369ae4161
arm64v1win=0f85c53f41827ea6a52c00d7a614cf195553e3a842face3e8c64bbffd9b7da85
arm64v1mingw=0f85c53f41827ea6a52c00d7a614cf195553e3a842face3e8c64bbffd9b7da85
arm64v1linux=a78207f09d5592174719f1648d0aea7b39ee19590f4ef96ea661c12369ae4161
arm64v1musl=a78207f09d5592174719f1648d0aea7b39ee19590f4ef96ea661c12369ae4161
arm64v1glibc=a78207f09d5592174719f1648d0aea7b39ee19590f4ef96ea661c12369ae4161
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
