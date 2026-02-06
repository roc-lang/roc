# META
~~~ini
description=String interpolation and concatenation
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

greeting = "Hello"
name = "World"
main = "${greeting}, ${name}!"
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : Str }
    exposes []
    packages {}
    provides { main_for_host: "main" }
    targets: {
        files: "targets/",
        exe: {
            x64glibc: [app],
        }
    }

main_for_host : Str
main_for_host = main
~~~
# MONO
~~~roc
# app
greeting = "Hello"
name = "World"
main = ""greeting", "name"!"

# platform
main_for_host = <required>

~~~
# DEV OUTPUT
~~~ini
x64mac=6e5acbb829e207d5d97cd4542a3f71476425319b51c7bdb9e75014f6ed89e427
x64win=3eb70b2902f950670996d8ea9afddac10d314a329430a87c333e0efd10ba8e91
x64freebsd=9f628b17f559fa894182e3e7c5667763df731613d0211758970782504f3db17e
x64openbsd=9f628b17f559fa894182e3e7c5667763df731613d0211758970782504f3db17e
x64netbsd=9f628b17f559fa894182e3e7c5667763df731613d0211758970782504f3db17e
x64musl=9f628b17f559fa894182e3e7c5667763df731613d0211758970782504f3db17e
x64glibc=9f628b17f559fa894182e3e7c5667763df731613d0211758970782504f3db17e
x64linux=9f628b17f559fa894182e3e7c5667763df731613d0211758970782504f3db17e
x64elf=9f628b17f559fa894182e3e7c5667763df731613d0211758970782504f3db17e
arm64mac=f61334a16fc0ba725c065f09fdcaf7460f0dd5b3be12e7c5c6ca1b531f808c11
arm64win=2e74f731859e7c3ac7590866f50762e14271109f84a37ee5cc2d5426fdd7637f
arm64linux=bc183093bc82d14858efe7e4801e4840091acc895e2e55b8fda1427705160553
arm64musl=bc183093bc82d14858efe7e4801e4840091acc895e2e55b8fda1427705160553
arm64glibc=bc183093bc82d14858efe7e4801e4840091acc895e2e55b8fda1427705160553
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
