# META
~~~ini
description=Integer arithmetic with I64 return type
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

main : I64
main = add(3, 4) * 2

add : I64, I64 -> I64
add = |a, b| a + b
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : I64 }
    exposes []
    packages {}
    provides { main_for_host: "main" }
    targets: {
        files: "targets/",
        exe: {
            x64glibc: [app],
        }
    }

main_for_host : I64
main_for_host = main
~~~
# MONO
~~~roc
# app
main = 14
add = |a, b| a + b

# platform
main_for_host = <required>

~~~
# DEV OUTPUT
~~~ini
x64mac=91b3018ca9b5917b012d13037f491d0fc13aaec3061dcfdd637ed37ad3494e76
x64win=0b5e64e57b68863e723c585610df017081a3a19aa68c5fade5d3000d0707dfc5
x64freebsd=7a95495a7047ed3a9be41e275d824ed354c0561cc50bf2fac34271bb691b3723
x64openbsd=7a95495a7047ed3a9be41e275d824ed354c0561cc50bf2fac34271bb691b3723
x64netbsd=7a95495a7047ed3a9be41e275d824ed354c0561cc50bf2fac34271bb691b3723
x64musl=7a95495a7047ed3a9be41e275d824ed354c0561cc50bf2fac34271bb691b3723
x64glibc=7a95495a7047ed3a9be41e275d824ed354c0561cc50bf2fac34271bb691b3723
x64linux=7a95495a7047ed3a9be41e275d824ed354c0561cc50bf2fac34271bb691b3723
x64elf=7a95495a7047ed3a9be41e275d824ed354c0561cc50bf2fac34271bb691b3723
arm64mac=4697edafd2a51fa07c83330817a3dcf1ca4d3fec12802317de5b5f761368db9d
arm64win=43ddd07684fce644ae3cc035c4fa9c34ce186343ee0b4fa5f146669302d2be9a
arm64linux=3efe373dea38985cf6963305e771af703dcef60a365b9769f95662713d2ee33a
arm64musl=3efe373dea38985cf6963305e771af703dcef60a365b9769f95662713d2ee33a
arm64glibc=3efe373dea38985cf6963305e771af703dcef60a365b9769f95662713d2ee33a
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
