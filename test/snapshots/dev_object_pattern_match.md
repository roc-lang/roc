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
    provides { "roc_main": main_for_host }
    targets: {
        inputs_dir: "targets/",
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
x64mac=5716a6ddb8d7e73982e555d4a9d9471ba985bb5b08aec64d8f3d8dda6a7780d1
x64win=61e483f450ede9f710d202a6ec39a0a006005ed8731fb690992e72f7d5006903
x64mingw=61e483f450ede9f710d202a6ec39a0a006005ed8731fb690992e72f7d5006903
x64freebsd=603a6b49947230fa2a7a13280a3821bb806e5ad679586538c7cbd6a64a4dd224
x64openbsd=d03ba29b76503ff802a568634ee14cf6c45807a851bcf98e97362bde9a8d5209
x64netbsd=5eea57abd91fae95d277c398fad3368b529743974b407c48e30658d5c011ee29
x64musl=5eea57abd91fae95d277c398fad3368b529743974b407c48e30658d5c011ee29
x64glibc=5eea57abd91fae95d277c398fad3368b529743974b407c48e30658d5c011ee29
x64linux=5eea57abd91fae95d277c398fad3368b529743974b407c48e30658d5c011ee29
x64elf=5eea57abd91fae95d277c398fad3368b529743974b407c48e30658d5c011ee29
x64v1mac=5716a6ddb8d7e73982e555d4a9d9471ba985bb5b08aec64d8f3d8dda6a7780d1
x64v1win=61e483f450ede9f710d202a6ec39a0a006005ed8731fb690992e72f7d5006903
x64v1mingw=61e483f450ede9f710d202a6ec39a0a006005ed8731fb690992e72f7d5006903
x64v1freebsd=603a6b49947230fa2a7a13280a3821bb806e5ad679586538c7cbd6a64a4dd224
x64v1openbsd=d03ba29b76503ff802a568634ee14cf6c45807a851bcf98e97362bde9a8d5209
x64v1netbsd=5eea57abd91fae95d277c398fad3368b529743974b407c48e30658d5c011ee29
x64v1musl=5eea57abd91fae95d277c398fad3368b529743974b407c48e30658d5c011ee29
x64v1glibc=5eea57abd91fae95d277c398fad3368b529743974b407c48e30658d5c011ee29
x64v1linux=5eea57abd91fae95d277c398fad3368b529743974b407c48e30658d5c011ee29
x64v1elf=5eea57abd91fae95d277c398fad3368b529743974b407c48e30658d5c011ee29
arm64mac=36236b01bdba01b0df335bb4191369f8fc2df03e752b33cd2b27d39786dfcfef
arm64win=cb854ccd28a915cf5e18c6006b73e1098535688b434ef2955e8aac5eda1f94c1
arm64mingw=cb854ccd28a915cf5e18c6006b73e1098535688b434ef2955e8aac5eda1f94c1
arm64linux=b7fdc97e2c5ce4af9445a2b054cb3911f10d3282dfd056b0d4cc61eaadfc96b9
arm64musl=b7fdc97e2c5ce4af9445a2b054cb3911f10d3282dfd056b0d4cc61eaadfc96b9
arm64glibc=b7fdc97e2c5ce4af9445a2b054cb3911f10d3282dfd056b0d4cc61eaadfc96b9
arm64v1win=cb854ccd28a915cf5e18c6006b73e1098535688b434ef2955e8aac5eda1f94c1
arm64v1mingw=cb854ccd28a915cf5e18c6006b73e1098535688b434ef2955e8aac5eda1f94c1
arm64v1linux=b7fdc97e2c5ce4af9445a2b054cb3911f10d3282dfd056b0d4cc61eaadfc96b9
arm64v1musl=b7fdc97e2c5ce4af9445a2b054cb3911f10d3282dfd056b0d4cc61eaadfc96b9
arm64v1glibc=b7fdc97e2c5ce4af9445a2b054cb3911f10d3282dfd056b0d4cc61eaadfc96b9
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
