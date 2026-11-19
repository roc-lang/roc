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
x64mac=8971bf165cf63b71e388572f180c746d9fa97dd7e28f2a00d458dbe49e8c534e
x64win=7088db5b585f7291e4ad77184891d5c6763c8324a3e17a242b3d0b36d2e17c2d
x64mingw=7088db5b585f7291e4ad77184891d5c6763c8324a3e17a242b3d0b36d2e17c2d
x64freebsd=4791b663ad3bb106a0e591892ca13b45dbc5054558c3a0740f18bf02be4df315
x64openbsd=9b40d40b99e238c8b472da9c931e0fdfcd4da119b901816ff847f1f37c3ebd11
x64netbsd=855497e19619b93b520f0512478b2ec47442d4fe5f1cb1bde8ea5c8464ccd9ea
x64musl=855497e19619b93b520f0512478b2ec47442d4fe5f1cb1bde8ea5c8464ccd9ea
x64glibc=855497e19619b93b520f0512478b2ec47442d4fe5f1cb1bde8ea5c8464ccd9ea
x64linux=855497e19619b93b520f0512478b2ec47442d4fe5f1cb1bde8ea5c8464ccd9ea
x64elf=855497e19619b93b520f0512478b2ec47442d4fe5f1cb1bde8ea5c8464ccd9ea
x64v1mac=8971bf165cf63b71e388572f180c746d9fa97dd7e28f2a00d458dbe49e8c534e
x64v1win=7088db5b585f7291e4ad77184891d5c6763c8324a3e17a242b3d0b36d2e17c2d
x64v1mingw=7088db5b585f7291e4ad77184891d5c6763c8324a3e17a242b3d0b36d2e17c2d
x64v1freebsd=4791b663ad3bb106a0e591892ca13b45dbc5054558c3a0740f18bf02be4df315
x64v1openbsd=9b40d40b99e238c8b472da9c931e0fdfcd4da119b901816ff847f1f37c3ebd11
x64v1netbsd=855497e19619b93b520f0512478b2ec47442d4fe5f1cb1bde8ea5c8464ccd9ea
x64v1musl=855497e19619b93b520f0512478b2ec47442d4fe5f1cb1bde8ea5c8464ccd9ea
x64v1glibc=855497e19619b93b520f0512478b2ec47442d4fe5f1cb1bde8ea5c8464ccd9ea
x64v1linux=855497e19619b93b520f0512478b2ec47442d4fe5f1cb1bde8ea5c8464ccd9ea
x64v1elf=855497e19619b93b520f0512478b2ec47442d4fe5f1cb1bde8ea5c8464ccd9ea
arm64mac=86c6492b9bd4238ddff5fc7ece0c1c3a960cddbaddb3e4e167be3428844d1469
arm64win=c175d37d8350e67061fd3e32c5d9a733d38cbd878f491aade350ba6fc05c2cf9
arm64mingw=c175d37d8350e67061fd3e32c5d9a733d38cbd878f491aade350ba6fc05c2cf9
arm64linux=24fdf689bb45821363127b2ff2956ecfae2268f4438b0235971e1adde899cf41
arm64musl=24fdf689bb45821363127b2ff2956ecfae2268f4438b0235971e1adde899cf41
arm64glibc=24fdf689bb45821363127b2ff2956ecfae2268f4438b0235971e1adde899cf41
arm64v1win=c175d37d8350e67061fd3e32c5d9a733d38cbd878f491aade350ba6fc05c2cf9
arm64v1mingw=c175d37d8350e67061fd3e32c5d9a733d38cbd878f491aade350ba6fc05c2cf9
arm64v1linux=24fdf689bb45821363127b2ff2956ecfae2268f4438b0235971e1adde899cf41
arm64v1musl=24fdf689bb45821363127b2ff2956ecfae2268f4438b0235971e1adde899cf41
arm64v1glibc=24fdf689bb45821363127b2ff2956ecfae2268f4438b0235971e1adde899cf41
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
