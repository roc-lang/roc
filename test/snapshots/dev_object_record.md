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
x64mac=ca70416ef80a18934eacaaaa553c3263fd3f700724df2ca2cbcc69b374557030
x64win=d19c89d7fe49728d00c8f76a576326c851e0f2adeb3d927bbd0bcb5ed171fda6
x64mingw=d19c89d7fe49728d00c8f76a576326c851e0f2adeb3d927bbd0bcb5ed171fda6
x64freebsd=a39a5cfc5ae3e2bb09877fea92699bef6c6207ad8c2553a9f501b2fe8d65abc4
x64openbsd=b3d00d4a49e9aadcc13ce83d587b5a2a53459507ea690fe683dab64b602ded62
x64netbsd=222fbda05fe167edf698d50257267e535fa2696d3b504c73f47c059b60e0632b
x64musl=222fbda05fe167edf698d50257267e535fa2696d3b504c73f47c059b60e0632b
x64glibc=222fbda05fe167edf698d50257267e535fa2696d3b504c73f47c059b60e0632b
x64linux=222fbda05fe167edf698d50257267e535fa2696d3b504c73f47c059b60e0632b
x64elf=222fbda05fe167edf698d50257267e535fa2696d3b504c73f47c059b60e0632b
x64v1mac=ca70416ef80a18934eacaaaa553c3263fd3f700724df2ca2cbcc69b374557030
x64v1win=d19c89d7fe49728d00c8f76a576326c851e0f2adeb3d927bbd0bcb5ed171fda6
x64v1mingw=d19c89d7fe49728d00c8f76a576326c851e0f2adeb3d927bbd0bcb5ed171fda6
x64v1freebsd=a39a5cfc5ae3e2bb09877fea92699bef6c6207ad8c2553a9f501b2fe8d65abc4
x64v1openbsd=b3d00d4a49e9aadcc13ce83d587b5a2a53459507ea690fe683dab64b602ded62
x64v1netbsd=222fbda05fe167edf698d50257267e535fa2696d3b504c73f47c059b60e0632b
x64v1musl=222fbda05fe167edf698d50257267e535fa2696d3b504c73f47c059b60e0632b
x64v1glibc=222fbda05fe167edf698d50257267e535fa2696d3b504c73f47c059b60e0632b
x64v1linux=222fbda05fe167edf698d50257267e535fa2696d3b504c73f47c059b60e0632b
x64v1elf=222fbda05fe167edf698d50257267e535fa2696d3b504c73f47c059b60e0632b
arm64mac=746f162aaa4c50e809b3349c2cfb9aeff573cde3bd24b379611504afe36924f7
arm64win=2a6802c60ccb802cfef47bafcc327dcdd7c93985b885100a4c0da09d700580c4
arm64mingw=2a6802c60ccb802cfef47bafcc327dcdd7c93985b885100a4c0da09d700580c4
arm64linux=145bf7ecddbcbfd5ed06053232d390517b560f7d0d73a3abf0d8690a41e3f0bc
arm64musl=145bf7ecddbcbfd5ed06053232d390517b560f7d0d73a3abf0d8690a41e3f0bc
arm64glibc=145bf7ecddbcbfd5ed06053232d390517b560f7d0d73a3abf0d8690a41e3f0bc
arm64v1win=2a6802c60ccb802cfef47bafcc327dcdd7c93985b885100a4c0da09d700580c4
arm64v1mingw=2a6802c60ccb802cfef47bafcc327dcdd7c93985b885100a4c0da09d700580c4
arm64v1linux=145bf7ecddbcbfd5ed06053232d390517b560f7d0d73a3abf0d8690a41e3f0bc
arm64v1musl=145bf7ecddbcbfd5ed06053232d390517b560f7d0d73a3abf0d8690a41e3f0bc
arm64v1glibc=145bf7ecddbcbfd5ed06053232d390517b560f7d0d73a3abf0d8690a41e3f0bc
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
