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
        inputs: "targets/",
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
x64mac=c91f295a6d2309d81f0df21fdb038a2e21d60b30422bef7439d6c0b064904c67
x64win=4dfd3f3d9b75177e286b920d773d62d0ed4b878472313124ea258a3513900c27
x64freebsd=b33e8b2c8dd940971349e46aa86485bfbd8f5bdfa81919cb3df3e5fddcaecc90
x64openbsd=b33e8b2c8dd940971349e46aa86485bfbd8f5bdfa81919cb3df3e5fddcaecc90
x64netbsd=b33e8b2c8dd940971349e46aa86485bfbd8f5bdfa81919cb3df3e5fddcaecc90
x64musl=b33e8b2c8dd940971349e46aa86485bfbd8f5bdfa81919cb3df3e5fddcaecc90
x64glibc=b33e8b2c8dd940971349e46aa86485bfbd8f5bdfa81919cb3df3e5fddcaecc90
x64linux=b33e8b2c8dd940971349e46aa86485bfbd8f5bdfa81919cb3df3e5fddcaecc90
x64elf=b33e8b2c8dd940971349e46aa86485bfbd8f5bdfa81919cb3df3e5fddcaecc90
arm64mac=b3075a0529a6b970e28ea97b279652c06c0408acf46b9c0d02c01f03262dec59
arm64win=98046472bc39291938343318717cdecc440e0eca7a58e896b244b28f62f6b1d8
arm64linux=819cb7f30122b5ecc3b7a1848c4c399a923cad1d47d35718dac3552953581ec3
arm64musl=819cb7f30122b5ecc3b7a1848c4c399a923cad1d47d35718dac3552953581ec3
arm64glibc=819cb7f30122b5ecc3b7a1848c4c399a923cad1d47d35718dac3552953581ec3
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
