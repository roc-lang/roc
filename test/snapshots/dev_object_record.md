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
x64mac=73041ee0e146642208f9a4bfcf8ebff1f416925fbe60e3cab96e145e21952f15
x64win=acda1a6e7662a55d1b06ce16ae52ea738f8f6ea3f52597e701628c9798680e73
x64mingw=acda1a6e7662a55d1b06ce16ae52ea738f8f6ea3f52597e701628c9798680e73
x64freebsd=740d07e56975b7e260c3076da3bab0710dd718c0cbd3fce0bce952664e0431e7
x64openbsd=305b6a1c92b70bb6b3981f34a9d864c859e8d5b9532cfd8dd862d5eadfd05335
x64netbsd=f9b96e744fe6695ceeefda41f7ff14dd119d83184c6db41933d5d4cb9fd64db1
x64musl=f9b96e744fe6695ceeefda41f7ff14dd119d83184c6db41933d5d4cb9fd64db1
x64glibc=f9b96e744fe6695ceeefda41f7ff14dd119d83184c6db41933d5d4cb9fd64db1
x64linux=f9b96e744fe6695ceeefda41f7ff14dd119d83184c6db41933d5d4cb9fd64db1
x64elf=f9b96e744fe6695ceeefda41f7ff14dd119d83184c6db41933d5d4cb9fd64db1
x64v1mac=73041ee0e146642208f9a4bfcf8ebff1f416925fbe60e3cab96e145e21952f15
x64v1win=acda1a6e7662a55d1b06ce16ae52ea738f8f6ea3f52597e701628c9798680e73
x64v1mingw=acda1a6e7662a55d1b06ce16ae52ea738f8f6ea3f52597e701628c9798680e73
x64v1freebsd=740d07e56975b7e260c3076da3bab0710dd718c0cbd3fce0bce952664e0431e7
x64v1openbsd=305b6a1c92b70bb6b3981f34a9d864c859e8d5b9532cfd8dd862d5eadfd05335
x64v1netbsd=f9b96e744fe6695ceeefda41f7ff14dd119d83184c6db41933d5d4cb9fd64db1
x64v1musl=f9b96e744fe6695ceeefda41f7ff14dd119d83184c6db41933d5d4cb9fd64db1
x64v1glibc=f9b96e744fe6695ceeefda41f7ff14dd119d83184c6db41933d5d4cb9fd64db1
x64v1linux=f9b96e744fe6695ceeefda41f7ff14dd119d83184c6db41933d5d4cb9fd64db1
x64v1elf=f9b96e744fe6695ceeefda41f7ff14dd119d83184c6db41933d5d4cb9fd64db1
arm64mac=904f7a02ece521e76ded51c03f945f7e8d4296700f417c70a47d54078370d604
arm64win=ff6c2890d9d82bdf2480c79053d16fba25461001775903461bdc7893c40288ed
arm64mingw=ff6c2890d9d82bdf2480c79053d16fba25461001775903461bdc7893c40288ed
arm64linux=5926f176b3e4643a88a557008100f9d03fee733527bf0b5f67b2702605803b37
arm64musl=5926f176b3e4643a88a557008100f9d03fee733527bf0b5f67b2702605803b37
arm64glibc=5926f176b3e4643a88a557008100f9d03fee733527bf0b5f67b2702605803b37
arm64v1win=ff6c2890d9d82bdf2480c79053d16fba25461001775903461bdc7893c40288ed
arm64v1mingw=ff6c2890d9d82bdf2480c79053d16fba25461001775903461bdc7893c40288ed
arm64v1linux=5926f176b3e4643a88a557008100f9d03fee733527bf0b5f67b2702605803b37
arm64v1musl=5926f176b3e4643a88a557008100f9d03fee733527bf0b5f67b2702605803b37
arm64v1glibc=5926f176b3e4643a88a557008100f9d03fee733527bf0b5f67b2702605803b37
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
