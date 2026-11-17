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
x64mac=68afc92a4602c45454217f2ebd5129bf478e4a68cdde5529e5c627deca9dd19a
x64win=ed68f8e8d690468c1a1cba3ec53631850e279658e1258890e38a79bba5fb57ba
x64mingw=ed68f8e8d690468c1a1cba3ec53631850e279658e1258890e38a79bba5fb57ba
x64freebsd=31502173ca68231f2b7e87b4fdcfeac69f682da7892ddd0073f4d7585705e62a
x64openbsd=1ef53c3e55098f7a608d10e5bc540b406e6c6257470329db47a20426f2a7799f
x64netbsd=b470eef2f322f6e3dbb0d555f4872027b1fc80b43f19400188a5090e01c59a80
x64musl=b470eef2f322f6e3dbb0d555f4872027b1fc80b43f19400188a5090e01c59a80
x64glibc=b470eef2f322f6e3dbb0d555f4872027b1fc80b43f19400188a5090e01c59a80
x64linux=b470eef2f322f6e3dbb0d555f4872027b1fc80b43f19400188a5090e01c59a80
x64elf=b470eef2f322f6e3dbb0d555f4872027b1fc80b43f19400188a5090e01c59a80
x64v1mac=68afc92a4602c45454217f2ebd5129bf478e4a68cdde5529e5c627deca9dd19a
x64v1win=ed68f8e8d690468c1a1cba3ec53631850e279658e1258890e38a79bba5fb57ba
x64v1mingw=ed68f8e8d690468c1a1cba3ec53631850e279658e1258890e38a79bba5fb57ba
x64v1freebsd=31502173ca68231f2b7e87b4fdcfeac69f682da7892ddd0073f4d7585705e62a
x64v1openbsd=1ef53c3e55098f7a608d10e5bc540b406e6c6257470329db47a20426f2a7799f
x64v1netbsd=b470eef2f322f6e3dbb0d555f4872027b1fc80b43f19400188a5090e01c59a80
x64v1musl=b470eef2f322f6e3dbb0d555f4872027b1fc80b43f19400188a5090e01c59a80
x64v1glibc=b470eef2f322f6e3dbb0d555f4872027b1fc80b43f19400188a5090e01c59a80
x64v1linux=b470eef2f322f6e3dbb0d555f4872027b1fc80b43f19400188a5090e01c59a80
x64v1elf=b470eef2f322f6e3dbb0d555f4872027b1fc80b43f19400188a5090e01c59a80
arm64mac=935151dbc9cae5d3486a3ffaca166011d46cbe3aaa0122f91a2159c4987d2e31
arm64win=312f114b6dec7620dd4486339455e4d586e3a498c940d26800a835d084f8f125
arm64mingw=312f114b6dec7620dd4486339455e4d586e3a498c940d26800a835d084f8f125
arm64linux=bc83319ff0d8eea41210cca68bacb08855dde5730fae84b6f35d217d1cbd34e8
arm64musl=bc83319ff0d8eea41210cca68bacb08855dde5730fae84b6f35d217d1cbd34e8
arm64glibc=bc83319ff0d8eea41210cca68bacb08855dde5730fae84b6f35d217d1cbd34e8
arm64v1win=312f114b6dec7620dd4486339455e4d586e3a498c940d26800a835d084f8f125
arm64v1mingw=312f114b6dec7620dd4486339455e4d586e3a498c940d26800a835d084f8f125
arm64v1linux=bc83319ff0d8eea41210cca68bacb08855dde5730fae84b6f35d217d1cbd34e8
arm64v1musl=bc83319ff0d8eea41210cca68bacb08855dde5730fae84b6f35d217d1cbd34e8
arm64v1glibc=bc83319ff0d8eea41210cca68bacb08855dde5730fae84b6f35d217d1cbd34e8
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
