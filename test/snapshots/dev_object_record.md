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
x64mac=8e56947c55c435c905adba7ac42bc02d539a9cca769996820d7a1a0172634edf
x64win=b2d5717a64f8ed37e43cca9f826c6d784eae8098db7453c7fd7ae1de9911bf98
x64mingw=b2d5717a64f8ed37e43cca9f826c6d784eae8098db7453c7fd7ae1de9911bf98
x64freebsd=bc429930fede7312cfa1652ee564e28f5ce6f91c7594b3f66d422aaa57ecb7d5
x64openbsd=3e5918ea8c3cbd4f173af5350da99da4f30ffa1c6cb939a1d1601626edbb4822
x64netbsd=1f7bd435a69f7bac836cbff45e136236c07ac0973fe58a7c329064dcf971abf6
x64musl=1f7bd435a69f7bac836cbff45e136236c07ac0973fe58a7c329064dcf971abf6
x64glibc=1f7bd435a69f7bac836cbff45e136236c07ac0973fe58a7c329064dcf971abf6
x64linux=1f7bd435a69f7bac836cbff45e136236c07ac0973fe58a7c329064dcf971abf6
x64elf=1f7bd435a69f7bac836cbff45e136236c07ac0973fe58a7c329064dcf971abf6
x64v1mac=8e56947c55c435c905adba7ac42bc02d539a9cca769996820d7a1a0172634edf
x64v1win=b2d5717a64f8ed37e43cca9f826c6d784eae8098db7453c7fd7ae1de9911bf98
x64v1mingw=b2d5717a64f8ed37e43cca9f826c6d784eae8098db7453c7fd7ae1de9911bf98
x64v1freebsd=bc429930fede7312cfa1652ee564e28f5ce6f91c7594b3f66d422aaa57ecb7d5
x64v1openbsd=3e5918ea8c3cbd4f173af5350da99da4f30ffa1c6cb939a1d1601626edbb4822
x64v1netbsd=1f7bd435a69f7bac836cbff45e136236c07ac0973fe58a7c329064dcf971abf6
x64v1musl=1f7bd435a69f7bac836cbff45e136236c07ac0973fe58a7c329064dcf971abf6
x64v1glibc=1f7bd435a69f7bac836cbff45e136236c07ac0973fe58a7c329064dcf971abf6
x64v1linux=1f7bd435a69f7bac836cbff45e136236c07ac0973fe58a7c329064dcf971abf6
x64v1elf=1f7bd435a69f7bac836cbff45e136236c07ac0973fe58a7c329064dcf971abf6
arm64mac=49f68bcf99c09157f0040b2cde3d0b72dd351abd21eed224de14501d6cb109ef
arm64win=3c43f5063c52e93674930bd7a07e6a6ccabc2ee4981c576a1f64fe1038b950c2
arm64mingw=3c43f5063c52e93674930bd7a07e6a6ccabc2ee4981c576a1f64fe1038b950c2
arm64linux=1cddc2b53e5ccab065785a8191f0bb476ff9b4106d11d7cb3c2d250b0851f8b3
arm64musl=1cddc2b53e5ccab065785a8191f0bb476ff9b4106d11d7cb3c2d250b0851f8b3
arm64glibc=1cddc2b53e5ccab065785a8191f0bb476ff9b4106d11d7cb3c2d250b0851f8b3
arm64v1win=3c43f5063c52e93674930bd7a07e6a6ccabc2ee4981c576a1f64fe1038b950c2
arm64v1mingw=3c43f5063c52e93674930bd7a07e6a6ccabc2ee4981c576a1f64fe1038b950c2
arm64v1linux=1cddc2b53e5ccab065785a8191f0bb476ff9b4106d11d7cb3c2d250b0851f8b3
arm64v1musl=1cddc2b53e5ccab065785a8191f0bb476ff9b4106d11d7cb3c2d250b0851f8b3
arm64v1glibc=1cddc2b53e5ccab065785a8191f0bb476ff9b4106d11d7cb3c2d250b0851f8b3
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
