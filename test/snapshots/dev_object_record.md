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
x64mac=3733cd1da012170e34d15d55b9f15ce583a64299bd7a6323bf9ba0bb3f2eab81
x64win=2a44b5672373d83aee48fd494284d915bb90929435fd6d7acecc091e275aa675
x64freebsd=cb139282a650483726429d369dec883176ccab834f6be0664e21fea7be25ae58
x64openbsd=cb139282a650483726429d369dec883176ccab834f6be0664e21fea7be25ae58
x64netbsd=cb139282a650483726429d369dec883176ccab834f6be0664e21fea7be25ae58
x64musl=cb139282a650483726429d369dec883176ccab834f6be0664e21fea7be25ae58
x64glibc=cb139282a650483726429d369dec883176ccab834f6be0664e21fea7be25ae58
x64linux=cb139282a650483726429d369dec883176ccab834f6be0664e21fea7be25ae58
x64elf=cb139282a650483726429d369dec883176ccab834f6be0664e21fea7be25ae58
arm64mac=2998adde7c8453e165ebf29c63dbf0f605a3c8d5b70fd96577020a6358e4b6c7
arm64win=33e3fe3b68f512c48d8ad5cb7f1152b0f8278541eb627a774f6ade98aa814161
arm64linux=7d8fabbd2ac4ed2df4d2767aced91a8afd907dd2c42338e8c7e4ceb24e8a9d7a
arm64musl=7d8fabbd2ac4ed2df4d2767aced91a8afd907dd2c42338e8c7e4ceb24e8a9d7a
arm64glibc=7d8fabbd2ac4ed2df4d2767aced91a8afd907dd2c42338e8c7e4ceb24e8a9d7a
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
