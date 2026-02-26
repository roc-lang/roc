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
    provides { name_for_host: "name", score_for_host: "score" }
    targets: {
        files: "targets/",
        exe: {
            x64glibc: [app],
        }
    }

name_for_host : Str
name_for_host = name

score_for_host : I64
score_for_host = score
~~~
# MONO
~~~roc
# app
name = "Alice"
score = 42

# platform
name_for_host = <required>
score_for_host = <required>

~~~
# DEV OUTPUT
~~~ini
x64mac=7519b8d88dcf437b8f99879df2572f03ab7bd6d0d79969c318de3fcd2f5579b6
x64win=4ddb7f4695ceda408acd3ee4a7b26f18533ac7938d13e9fdff305ab9ea98ea78
x64freebsd=ca2abb191a1be909e7f72600fbfc1dc4b842500c8aa2033df697581b828a95f2
x64openbsd=ca2abb191a1be909e7f72600fbfc1dc4b842500c8aa2033df697581b828a95f2
x64netbsd=ca2abb191a1be909e7f72600fbfc1dc4b842500c8aa2033df697581b828a95f2
x64musl=ca2abb191a1be909e7f72600fbfc1dc4b842500c8aa2033df697581b828a95f2
x64glibc=ca2abb191a1be909e7f72600fbfc1dc4b842500c8aa2033df697581b828a95f2
x64linux=ca2abb191a1be909e7f72600fbfc1dc4b842500c8aa2033df697581b828a95f2
x64elf=ca2abb191a1be909e7f72600fbfc1dc4b842500c8aa2033df697581b828a95f2
arm64mac=2998adde7c8453e165ebf29c63dbf0f605a3c8d5b70fd96577020a6358e4b6c7
arm64win=33e3fe3b68f512c48d8ad5cb7f1152b0f8278541eb627a774f6ade98aa814161
arm64linux=7d8fabbd2ac4ed2df4d2767aced91a8afd907dd2c42338e8c7e4ceb24e8a9d7a
arm64musl=7d8fabbd2ac4ed2df4d2767aced91a8afd907dd2c42338e8c7e4ceb24e8a9d7a
arm64glibc=7d8fabbd2ac4ed2df4d2767aced91a8afd907dd2c42338e8c7e4ceb24e8a9d7a
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
