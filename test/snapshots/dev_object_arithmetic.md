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
    provides { "roc_main": main_for_host }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
    }

main_for_host : I64
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# app
main = add(3, 4) * 2
add = |a, b| a + b

~~~
# DEV OUTPUT
~~~ini
x64mac=860b012c69e391389f08593a77a9fb5b16f91982b5995dac1b55baea30cc948d
x64win=0141981f85bf579e5e518e6a9d7bd9f959da6011b54065fd1b125edcfcce381e
x64mingw=0141981f85bf579e5e518e6a9d7bd9f959da6011b54065fd1b125edcfcce381e
x64freebsd=17de390c1e95d352dcb0c1e1d790293fb80e75842a6793ac9ba54b7c7d617275
x64openbsd=ba167ba667ec3024711719e60bcd488af10e77a350babc5629133a8e2da3da15
x64netbsd=9a1ad222305c47e299f08335dd2eb8e6cf93104c304bee4ca9a3fcbcd2f9a030
x64musl=9a1ad222305c47e299f08335dd2eb8e6cf93104c304bee4ca9a3fcbcd2f9a030
x64glibc=9a1ad222305c47e299f08335dd2eb8e6cf93104c304bee4ca9a3fcbcd2f9a030
x64linux=9a1ad222305c47e299f08335dd2eb8e6cf93104c304bee4ca9a3fcbcd2f9a030
x64elf=9a1ad222305c47e299f08335dd2eb8e6cf93104c304bee4ca9a3fcbcd2f9a030
x64v1mac=860b012c69e391389f08593a77a9fb5b16f91982b5995dac1b55baea30cc948d
x64v1win=0141981f85bf579e5e518e6a9d7bd9f959da6011b54065fd1b125edcfcce381e
x64v1mingw=0141981f85bf579e5e518e6a9d7bd9f959da6011b54065fd1b125edcfcce381e
x64v1freebsd=17de390c1e95d352dcb0c1e1d790293fb80e75842a6793ac9ba54b7c7d617275
x64v1openbsd=ba167ba667ec3024711719e60bcd488af10e77a350babc5629133a8e2da3da15
x64v1netbsd=9a1ad222305c47e299f08335dd2eb8e6cf93104c304bee4ca9a3fcbcd2f9a030
x64v1musl=9a1ad222305c47e299f08335dd2eb8e6cf93104c304bee4ca9a3fcbcd2f9a030
x64v1glibc=9a1ad222305c47e299f08335dd2eb8e6cf93104c304bee4ca9a3fcbcd2f9a030
x64v1linux=9a1ad222305c47e299f08335dd2eb8e6cf93104c304bee4ca9a3fcbcd2f9a030
x64v1elf=9a1ad222305c47e299f08335dd2eb8e6cf93104c304bee4ca9a3fcbcd2f9a030
arm64mac=99030a794cc8f86ad82aad198e397af634961ac82eb6aaa90f1a76477c4dbc90
arm64win=f340822770f4f1e8198010f4e34dd9c9fb058cb9a4dbb7831433fc69b57418ab
arm64mingw=f340822770f4f1e8198010f4e34dd9c9fb058cb9a4dbb7831433fc69b57418ab
arm64linux=038988bdaacca05a74c63262911fd2db01b7ded7e467b10fefaa97c674c7d60f
arm64musl=038988bdaacca05a74c63262911fd2db01b7ded7e467b10fefaa97c674c7d60f
arm64glibc=038988bdaacca05a74c63262911fd2db01b7ded7e467b10fefaa97c674c7d60f
arm64v1win=f340822770f4f1e8198010f4e34dd9c9fb058cb9a4dbb7831433fc69b57418ab
arm64v1mingw=f340822770f4f1e8198010f4e34dd9c9fb058cb9a4dbb7831433fc69b57418ab
arm64v1linux=038988bdaacca05a74c63262911fd2db01b7ded7e467b10fefaa97c674c7d60f
arm64v1musl=038988bdaacca05a74c63262911fd2db01b7ded7e467b10fefaa97c674c7d60f
arm64v1glibc=038988bdaacca05a74c63262911fd2db01b7ded7e467b10fefaa97c674c7d60f
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
