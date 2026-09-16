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
x64mac=d83b6f546c201f788118af66a471c223440de140267008dd1fedb6038ac6f283
x64win=8b105778975fea1ada6561a6193a5cf04e160cef8b0359a67410ca9a11771b98
x64mingw=8b105778975fea1ada6561a6193a5cf04e160cef8b0359a67410ca9a11771b98
x64freebsd=a0a4a13c2ff2c5b735fa51e654a658513103c30b57576320745d22b03168738c
x64openbsd=e8ef80bda7324f88f80ca127e1684316ff3a9e24ef28ee5ac0015b9c20e97590
x64netbsd=a38d9ca7ba51ebff86a0fd07b51a6543a459106f880fbf75c75cb7a942ec5a9f
x64musl=a38d9ca7ba51ebff86a0fd07b51a6543a459106f880fbf75c75cb7a942ec5a9f
x64glibc=a38d9ca7ba51ebff86a0fd07b51a6543a459106f880fbf75c75cb7a942ec5a9f
x64linux=a38d9ca7ba51ebff86a0fd07b51a6543a459106f880fbf75c75cb7a942ec5a9f
x64elf=a38d9ca7ba51ebff86a0fd07b51a6543a459106f880fbf75c75cb7a942ec5a9f
x64v1mac=d83b6f546c201f788118af66a471c223440de140267008dd1fedb6038ac6f283
x64v1win=8b105778975fea1ada6561a6193a5cf04e160cef8b0359a67410ca9a11771b98
x64v1mingw=8b105778975fea1ada6561a6193a5cf04e160cef8b0359a67410ca9a11771b98
x64v1freebsd=a0a4a13c2ff2c5b735fa51e654a658513103c30b57576320745d22b03168738c
x64v1openbsd=e8ef80bda7324f88f80ca127e1684316ff3a9e24ef28ee5ac0015b9c20e97590
x64v1netbsd=a38d9ca7ba51ebff86a0fd07b51a6543a459106f880fbf75c75cb7a942ec5a9f
x64v1musl=a38d9ca7ba51ebff86a0fd07b51a6543a459106f880fbf75c75cb7a942ec5a9f
x64v1glibc=a38d9ca7ba51ebff86a0fd07b51a6543a459106f880fbf75c75cb7a942ec5a9f
x64v1linux=a38d9ca7ba51ebff86a0fd07b51a6543a459106f880fbf75c75cb7a942ec5a9f
x64v1elf=a38d9ca7ba51ebff86a0fd07b51a6543a459106f880fbf75c75cb7a942ec5a9f
arm64mac=4fa5b15498fde4e1fd73d2806dcf12f13d5828485bc1d4602c30eb7b4d25c9b5
arm64win=b5a04ec88049215532050f3df1f924865d2ac3f4cae51c5f1a54a1749c000fff
arm64mingw=b5a04ec88049215532050f3df1f924865d2ac3f4cae51c5f1a54a1749c000fff
arm64linux=cebd7a394ea03eeb2477b3b03d89696abd7e63a06c2e367eaa2ffb1362dc347a
arm64musl=cebd7a394ea03eeb2477b3b03d89696abd7e63a06c2e367eaa2ffb1362dc347a
arm64glibc=cebd7a394ea03eeb2477b3b03d89696abd7e63a06c2e367eaa2ffb1362dc347a
arm64v1win=b5a04ec88049215532050f3df1f924865d2ac3f4cae51c5f1a54a1749c000fff
arm64v1mingw=b5a04ec88049215532050f3df1f924865d2ac3f4cae51c5f1a54a1749c000fff
arm64v1linux=cebd7a394ea03eeb2477b3b03d89696abd7e63a06c2e367eaa2ffb1362dc347a
arm64v1musl=cebd7a394ea03eeb2477b3b03d89696abd7e63a06c2e367eaa2ffb1362dc347a
arm64v1glibc=cebd7a394ea03eeb2477b3b03d89696abd7e63a06c2e367eaa2ffb1362dc347a
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
