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
arm64mac=d88d46868ada1c515d7c219d87e4f1bfbd802bde4f90031fc45cfd0fd6c34d27
arm64win=8049bd2d6089842df4946ce53c129818aaa223511630f88ebd17aa30999b2c16
arm64linux=6581fb27a7d0f9574b8622d9a1fa13e70023e2b714fe1cd8c0d28623d818f120
arm64musl=6581fb27a7d0f9574b8622d9a1fa13e70023e2b714fe1cd8c0d28623d818f120
arm64glibc=6581fb27a7d0f9574b8622d9a1fa13e70023e2b714fe1cd8c0d28623d818f120
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
