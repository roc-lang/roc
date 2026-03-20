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
x64mac=36f40fb76b422bcd2b92484073e5bf269ecf12783ded877b9e97bcdc9c301a24
x64win=6b4041771e894f49b6d360642a473693a495e252084155979b77fca1c604e3db
x64freebsd=e03a948ce7fc3be3cd5d303337ae36166d929336fd0ec0b834e8115b15533984
x64openbsd=e03a948ce7fc3be3cd5d303337ae36166d929336fd0ec0b834e8115b15533984
x64netbsd=e03a948ce7fc3be3cd5d303337ae36166d929336fd0ec0b834e8115b15533984
x64musl=e03a948ce7fc3be3cd5d303337ae36166d929336fd0ec0b834e8115b15533984
x64glibc=e03a948ce7fc3be3cd5d303337ae36166d929336fd0ec0b834e8115b15533984
x64linux=e03a948ce7fc3be3cd5d303337ae36166d929336fd0ec0b834e8115b15533984
x64elf=e03a948ce7fc3be3cd5d303337ae36166d929336fd0ec0b834e8115b15533984
arm64mac=d5179df70fd47dda9d43d751a0a85937c0e93dfc8743ab9651acb2123a7b335e
arm64win=539188c265b3da35277a298a292e94ec9302c69722e41f4c6041d91943793eab
arm64linux=72b5e265d209ac774480b9ab8a25641f5679fc1ae13e930e75af86afd3eaa969
arm64musl=72b5e265d209ac774480b9ab8a25641f5679fc1ae13e930e75af86afd3eaa969
arm64glibc=72b5e265d209ac774480b9ab8a25641f5679fc1ae13e930e75af86afd3eaa969
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
