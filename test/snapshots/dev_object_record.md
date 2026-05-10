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
# platform
name_for_host = <required>
score_for_host = <required>

# app
name = "Alice"
score = 42

~~~
# DEV OUTPUT
~~~ini
x64mac=8a0973f34b7989b6830b0aef188855324d3457e0c0a21b3db912507fa7bd96b1
x64win=4dfd3f3d9b75177e286b920d773d62d0ed4b878472313124ea258a3513900c27
x64freebsd=171b82608b59330a95a5304988655d0e8858a05eb8bd86c83b82cd9c57fbe08a
x64openbsd=171b82608b59330a95a5304988655d0e8858a05eb8bd86c83b82cd9c57fbe08a
x64netbsd=171b82608b59330a95a5304988655d0e8858a05eb8bd86c83b82cd9c57fbe08a
x64musl=171b82608b59330a95a5304988655d0e8858a05eb8bd86c83b82cd9c57fbe08a
x64glibc=171b82608b59330a95a5304988655d0e8858a05eb8bd86c83b82cd9c57fbe08a
x64linux=171b82608b59330a95a5304988655d0e8858a05eb8bd86c83b82cd9c57fbe08a
x64elf=171b82608b59330a95a5304988655d0e8858a05eb8bd86c83b82cd9c57fbe08a
arm64mac=c164e5be92fe5a50a6e7ea78954edec59d3ca9fdd572b968ff884ec47a80dac0
arm64win=98046472bc39291938343318717cdecc440e0eca7a58e896b244b28f62f6b1d8
arm64linux=fc1eb5d364ccd227b246f3e07460a5a9df1bd72cb0bac60c86bdb7d9222e6962
arm64musl=fc1eb5d364ccd227b246f3e07460a5a9df1bd72cb0bac60c86bdb7d9222e6962
arm64glibc=fc1eb5d364ccd227b246f3e07460a5a9df1bd72cb0bac60c86bdb7d9222e6962
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
