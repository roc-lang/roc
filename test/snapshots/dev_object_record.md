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
x64mac=db2d2bc865b168c6016086bfe88c9c0b5427f7a79a520d937b14c89120977acd
x64win=7af09c7b48337304e3db68a65757592d65b35f06bf76d68844af5fe207aeb83e
x64mingw=7af09c7b48337304e3db68a65757592d65b35f06bf76d68844af5fe207aeb83e
x64freebsd=99b5fae4589c9430b7c8dce7bc1ac0ddfdb58147a960ea464bdfc54fcca3821c
x64openbsd=9bba5e0e9543eac2c762074c1129e0c4e4b25d8c3f4f53854d68146aafd47fcc
x64netbsd=03869b9feea0ebf27029ddee10695c874351d53c0a5851b7ba602d350ef9582c
x64musl=03869b9feea0ebf27029ddee10695c874351d53c0a5851b7ba602d350ef9582c
x64glibc=03869b9feea0ebf27029ddee10695c874351d53c0a5851b7ba602d350ef9582c
x64linux=03869b9feea0ebf27029ddee10695c874351d53c0a5851b7ba602d350ef9582c
x64elf=03869b9feea0ebf27029ddee10695c874351d53c0a5851b7ba602d350ef9582c
x64v1mac=db2d2bc865b168c6016086bfe88c9c0b5427f7a79a520d937b14c89120977acd
x64v1win=7af09c7b48337304e3db68a65757592d65b35f06bf76d68844af5fe207aeb83e
x64v1mingw=7af09c7b48337304e3db68a65757592d65b35f06bf76d68844af5fe207aeb83e
x64v1freebsd=99b5fae4589c9430b7c8dce7bc1ac0ddfdb58147a960ea464bdfc54fcca3821c
x64v1openbsd=9bba5e0e9543eac2c762074c1129e0c4e4b25d8c3f4f53854d68146aafd47fcc
x64v1netbsd=03869b9feea0ebf27029ddee10695c874351d53c0a5851b7ba602d350ef9582c
x64v1musl=03869b9feea0ebf27029ddee10695c874351d53c0a5851b7ba602d350ef9582c
x64v1glibc=03869b9feea0ebf27029ddee10695c874351d53c0a5851b7ba602d350ef9582c
x64v1linux=03869b9feea0ebf27029ddee10695c874351d53c0a5851b7ba602d350ef9582c
x64v1elf=03869b9feea0ebf27029ddee10695c874351d53c0a5851b7ba602d350ef9582c
arm64mac=3babeac4b5d04d47cd4e17ebb4325c7f21b0ba5b7094c6aed3111603321fe000
arm64win=3901c48071aa182aa21209739be3e5bfe8f91d7c8f12dac24cd1094ea569c613
arm64mingw=3901c48071aa182aa21209739be3e5bfe8f91d7c8f12dac24cd1094ea569c613
arm64linux=74457c8a3b6f06b8cb8057365e09945b385a8e15371f07a11f9c3b9e42a7dd5f
arm64musl=74457c8a3b6f06b8cb8057365e09945b385a8e15371f07a11f9c3b9e42a7dd5f
arm64glibc=74457c8a3b6f06b8cb8057365e09945b385a8e15371f07a11f9c3b9e42a7dd5f
arm64v1win=3901c48071aa182aa21209739be3e5bfe8f91d7c8f12dac24cd1094ea569c613
arm64v1mingw=3901c48071aa182aa21209739be3e5bfe8f91d7c8f12dac24cd1094ea569c613
arm64v1linux=74457c8a3b6f06b8cb8057365e09945b385a8e15371f07a11f9c3b9e42a7dd5f
arm64v1musl=74457c8a3b6f06b8cb8057365e09945b385a8e15371f07a11f9c3b9e42a7dd5f
arm64v1glibc=74457c8a3b6f06b8cb8057365e09945b385a8e15371f07a11f9c3b9e42a7dd5f
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
