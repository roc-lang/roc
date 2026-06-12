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
x64mac=c3c6ea0cc9731bfd53a30b4b01933e1638468c59b307561ad857a07ee6b86ad6
x64win=e30ad5a97e1e6124552a0ab34085924cbf5375ea20c68a4fb357f15cad96e3ee
x64freebsd=004c96fe0227b3aaafb747ac9390761591a3a5d002c22c7418a5c5ad42362180
x64openbsd=004c96fe0227b3aaafb747ac9390761591a3a5d002c22c7418a5c5ad42362180
x64netbsd=004c96fe0227b3aaafb747ac9390761591a3a5d002c22c7418a5c5ad42362180
x64musl=004c96fe0227b3aaafb747ac9390761591a3a5d002c22c7418a5c5ad42362180
x64glibc=004c96fe0227b3aaafb747ac9390761591a3a5d002c22c7418a5c5ad42362180
x64linux=004c96fe0227b3aaafb747ac9390761591a3a5d002c22c7418a5c5ad42362180
x64elf=004c96fe0227b3aaafb747ac9390761591a3a5d002c22c7418a5c5ad42362180
arm64mac=7f1243ea63d6de6425d6c309f1636da2122f6549b3678760a100725dbbeced70
arm64win=03471e38e5d89948f0fa26131cc98b2a2fd963bc9fb7127a5a6e6a8c46142889
arm64linux=a0fed48341fd2c1aee6e30916e5e2cc6093ed8d3d391ded07f76cc889c07af8e
arm64musl=a0fed48341fd2c1aee6e30916e5e2cc6093ed8d3d391ded07f76cc889c07af8e
arm64glibc=a0fed48341fd2c1aee6e30916e5e2cc6093ed8d3d391ded07f76cc889c07af8e
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
