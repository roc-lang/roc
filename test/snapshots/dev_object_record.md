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
x64mac=4a4b75adc69d4075f797cddfa660c0ca8a5ae8e893ad2ea77dfcb364dd416ee8
x64win=fd131b7758c4a27292691ddba9475700f924ef56f81687715facf1620ee886c1
x64freebsd=a59551839e3cd06633dad82b0f36e735b5b04410bf7b8f03d60293292171ea3f
x64openbsd=a59551839e3cd06633dad82b0f36e735b5b04410bf7b8f03d60293292171ea3f
x64netbsd=a59551839e3cd06633dad82b0f36e735b5b04410bf7b8f03d60293292171ea3f
x64musl=a59551839e3cd06633dad82b0f36e735b5b04410bf7b8f03d60293292171ea3f
x64glibc=a59551839e3cd06633dad82b0f36e735b5b04410bf7b8f03d60293292171ea3f
x64linux=a59551839e3cd06633dad82b0f36e735b5b04410bf7b8f03d60293292171ea3f
x64elf=a59551839e3cd06633dad82b0f36e735b5b04410bf7b8f03d60293292171ea3f
arm64mac=944e410cc0c833b3f84d73176e0b7957156e6bf5616676c196c03f22c889b6a5
arm64win=6fe7a52103cea81471ab6751738eed21ba2c40c5724bea3d2ed3914d1ed28fc4
arm64linux=058eda16f996b41f3cbe75745c9b0241550bb38baf4a2a70a1fd760efd1d4299
arm64musl=058eda16f996b41f3cbe75745c9b0241550bb38baf4a2a70a1fd760efd1d4299
arm64glibc=058eda16f996b41f3cbe75745c9b0241550bb38baf4a2a70a1fd760efd1d4299
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
