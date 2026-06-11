# META
~~~ini
description=Tag unions and pattern matching
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

Color : [Red, Green, Blue]

to_str : Color -> Str
to_str = |color|
    match color {
        Red => "red"
        Green => "green"
        Blue => "blue"
    }

main = to_str(Red)
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : Str }
    exposes []
    packages {}
    provides { main_for_host: "main" }
    targets: {
        inputs: "targets/",
        x64glibc: { inputs: [app] },
    }

main_for_host : Str
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# app
to_str = |color| match color {
	Red => "red"
	Green => "green"
	Blue => "blue"
}
main = to_str(Red)

~~~
# DEV OUTPUT
~~~ini
x64mac=eb73f995f5dbd10f794135524cfdf515e11c692e5fc3f0e0afd3478c476d4ef8
x64win=1daef17641e043f3ad6337cdf62da40514a74d4dbd0790db66fb5b2a8d322c32
x64freebsd=3ec3194d32b51659f814ef019ceb7eec88fd5269cee976bfc3c0ca2aa4e7b14d
x64openbsd=3ec3194d32b51659f814ef019ceb7eec88fd5269cee976bfc3c0ca2aa4e7b14d
x64netbsd=3ec3194d32b51659f814ef019ceb7eec88fd5269cee976bfc3c0ca2aa4e7b14d
x64musl=3ec3194d32b51659f814ef019ceb7eec88fd5269cee976bfc3c0ca2aa4e7b14d
x64glibc=3ec3194d32b51659f814ef019ceb7eec88fd5269cee976bfc3c0ca2aa4e7b14d
x64linux=3ec3194d32b51659f814ef019ceb7eec88fd5269cee976bfc3c0ca2aa4e7b14d
x64elf=3ec3194d32b51659f814ef019ceb7eec88fd5269cee976bfc3c0ca2aa4e7b14d
arm64mac=6dc9f6a7190e30b876a15fea6048b3bedb820a3acdcb143371c2cdf1dde06e4d
arm64win=c25f1af5449d9203ccb6047a467adccde3caee7f094cfa7ddb0a7f7d201ba658
arm64linux=0cdc345ba24040913b279130488a1662c8a1ef9b69e995a9cacdab59bf06f74c
arm64musl=0cdc345ba24040913b279130488a1662c8a1ef9b69e995a9cacdab59bf06f74c
arm64glibc=0cdc345ba24040913b279130488a1662c8a1ef9b69e995a9cacdab59bf06f74c
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
