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
x64mac=56feb018091cb531c1558cbc335b056ace790d1397748530f23d32ee9489c3bc
x64win=bf6e454e2011913f1f467c12c298d7664fbee99b51d7939bf0e7887552dda366
x64freebsd=1869b95014df5a4d59a7130f639af353046acca3a0e392e6a0bd45bb5a0cacd0
x64openbsd=1869b95014df5a4d59a7130f639af353046acca3a0e392e6a0bd45bb5a0cacd0
x64netbsd=1869b95014df5a4d59a7130f639af353046acca3a0e392e6a0bd45bb5a0cacd0
x64musl=1869b95014df5a4d59a7130f639af353046acca3a0e392e6a0bd45bb5a0cacd0
x64glibc=1869b95014df5a4d59a7130f639af353046acca3a0e392e6a0bd45bb5a0cacd0
x64linux=1869b95014df5a4d59a7130f639af353046acca3a0e392e6a0bd45bb5a0cacd0
x64elf=1869b95014df5a4d59a7130f639af353046acca3a0e392e6a0bd45bb5a0cacd0
arm64mac=4697edafd2a51fa07c83330817a3dcf1ca4d3fec12802317de5b5f761368db9d
arm64win=43ddd07684fce644ae3cc035c4fa9c34ce186343ee0b4fa5f146669302d2be9a
arm64linux=3efe373dea38985cf6963305e771af703dcef60a365b9769f95662713d2ee33a
arm64musl=3efe373dea38985cf6963305e771af703dcef60a365b9769f95662713d2ee33a
arm64glibc=3efe373dea38985cf6963305e771af703dcef60a365b9769f95662713d2ee33a
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
