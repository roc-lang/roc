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
x64mac=a0ddd72c997ecbe0670633aa9588756f0da3b391ed4321fa8716c91a57c3436a
x64win=3c5553a8c37c2338a95a14f746f6945a82a4da5352cccbee563c4b437d270988
x64mingw=3c5553a8c37c2338a95a14f746f6945a82a4da5352cccbee563c4b437d270988
x64freebsd=9674d1eb2625273ded75ccaf1a2dcff3df0013b7db7effe49b41f6d16fcbdd17
x64openbsd=03d706f11be7d5cb3cbab8e625c853881a05cc2698aa2fbbdd3d5950c146ad5e
x64netbsd=72f62e956cfc5dc39c2e35cdd41ecc78df186eda6740e4bd732dc7c3546408fd
x64musl=72f62e956cfc5dc39c2e35cdd41ecc78df186eda6740e4bd732dc7c3546408fd
x64glibc=72f62e956cfc5dc39c2e35cdd41ecc78df186eda6740e4bd732dc7c3546408fd
x64linux=72f62e956cfc5dc39c2e35cdd41ecc78df186eda6740e4bd732dc7c3546408fd
x64elf=72f62e956cfc5dc39c2e35cdd41ecc78df186eda6740e4bd732dc7c3546408fd
x64v1mac=a0ddd72c997ecbe0670633aa9588756f0da3b391ed4321fa8716c91a57c3436a
x64v1win=3c5553a8c37c2338a95a14f746f6945a82a4da5352cccbee563c4b437d270988
x64v1mingw=3c5553a8c37c2338a95a14f746f6945a82a4da5352cccbee563c4b437d270988
x64v1freebsd=9674d1eb2625273ded75ccaf1a2dcff3df0013b7db7effe49b41f6d16fcbdd17
x64v1openbsd=03d706f11be7d5cb3cbab8e625c853881a05cc2698aa2fbbdd3d5950c146ad5e
x64v1netbsd=72f62e956cfc5dc39c2e35cdd41ecc78df186eda6740e4bd732dc7c3546408fd
x64v1musl=72f62e956cfc5dc39c2e35cdd41ecc78df186eda6740e4bd732dc7c3546408fd
x64v1glibc=72f62e956cfc5dc39c2e35cdd41ecc78df186eda6740e4bd732dc7c3546408fd
x64v1linux=72f62e956cfc5dc39c2e35cdd41ecc78df186eda6740e4bd732dc7c3546408fd
x64v1elf=72f62e956cfc5dc39c2e35cdd41ecc78df186eda6740e4bd732dc7c3546408fd
arm64mac=f8b4451039ad5c9c0ec19d8e7d2a3d95552f07a38a6c71003c3d92be7d5fd2cf
arm64win=84c854733fcbf9d8b4cdf27a7423fbb7f857f7576a1d3e828975947308fbc7ef
arm64mingw=84c854733fcbf9d8b4cdf27a7423fbb7f857f7576a1d3e828975947308fbc7ef
arm64linux=ac89a807a9d74848aabcd95c45501462e9d37ab8a4b534358657a3bd8cebc183
arm64musl=ac89a807a9d74848aabcd95c45501462e9d37ab8a4b534358657a3bd8cebc183
arm64glibc=ac89a807a9d74848aabcd95c45501462e9d37ab8a4b534358657a3bd8cebc183
arm64v1win=84c854733fcbf9d8b4cdf27a7423fbb7f857f7576a1d3e828975947308fbc7ef
arm64v1mingw=84c854733fcbf9d8b4cdf27a7423fbb7f857f7576a1d3e828975947308fbc7ef
arm64v1linux=ac89a807a9d74848aabcd95c45501462e9d37ab8a4b534358657a3bd8cebc183
arm64v1musl=ac89a807a9d74848aabcd95c45501462e9d37ab8a4b534358657a3bd8cebc183
arm64v1glibc=ac89a807a9d74848aabcd95c45501462e9d37ab8a4b534358657a3bd8cebc183
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
