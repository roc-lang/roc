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
x64mac=02bf7ecd33320bae6132bb37d731830d2e5897c0806b8bc48b50a11fe91a2015
x64win=78da1105951f668ca76b6311a9eb26aa17077697a16afb9b7b8c208f34ed14bd
x64freebsd=efd04aad30f6332688a2229e27dccfb8daae9c3d891894dfb12b60230466188a
x64openbsd=efd04aad30f6332688a2229e27dccfb8daae9c3d891894dfb12b60230466188a
x64netbsd=efd04aad30f6332688a2229e27dccfb8daae9c3d891894dfb12b60230466188a
x64musl=efd04aad30f6332688a2229e27dccfb8daae9c3d891894dfb12b60230466188a
x64glibc=efd04aad30f6332688a2229e27dccfb8daae9c3d891894dfb12b60230466188a
x64linux=efd04aad30f6332688a2229e27dccfb8daae9c3d891894dfb12b60230466188a
x64elf=efd04aad30f6332688a2229e27dccfb8daae9c3d891894dfb12b60230466188a
arm64mac=d5179df70fd47dda9d43d751a0a85937c0e93dfc8743ab9651acb2123a7b335e
arm64win=539188c265b3da35277a298a292e94ec9302c69722e41f4c6041d91943793eab
arm64linux=72b5e265d209ac774480b9ab8a25641f5679fc1ae13e930e75af86afd3eaa969
arm64musl=72b5e265d209ac774480b9ab8a25641f5679fc1ae13e930e75af86afd3eaa969
arm64glibc=72b5e265d209ac774480b9ab8a25641f5679fc1ae13e930e75af86afd3eaa969
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
