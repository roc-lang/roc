# META
~~~ini
description=String interpolation and concatenation
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

greeting = "Hello"
name = "World"
main = "${greeting}, ${name}!"
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : Str }
    exposes []
    packages {}
    provides { main_for_host: "main" }
    targets: {
        files: "targets/",
        exe: {
            x64glibc: [app],
        }
    }

main_for_host : Str
main_for_host = main
~~~
# MONO
~~~roc
# app
greeting = "Hello"
name = "World"
main = ""greeting", "name"!"

# platform
main_for_host = <required>

~~~
# DEV OUTPUT
~~~ini
x64mac=a912af83337c7e5961da5358367c23120184550172f5a42629b17d943d2231b9
x64win=1d43b71c98235d6214ddf509bf555ef08b50c8a5f25ba857f07c5385a6509d1d
x64freebsd=af140384a72a0e62e5b0248f98f8b8747d63fb701e7a00f0443ed5dbfb9a0428
x64openbsd=af140384a72a0e62e5b0248f98f8b8747d63fb701e7a00f0443ed5dbfb9a0428
x64netbsd=af140384a72a0e62e5b0248f98f8b8747d63fb701e7a00f0443ed5dbfb9a0428
x64musl=af140384a72a0e62e5b0248f98f8b8747d63fb701e7a00f0443ed5dbfb9a0428
x64glibc=af140384a72a0e62e5b0248f98f8b8747d63fb701e7a00f0443ed5dbfb9a0428
x64linux=af140384a72a0e62e5b0248f98f8b8747d63fb701e7a00f0443ed5dbfb9a0428
x64elf=af140384a72a0e62e5b0248f98f8b8747d63fb701e7a00f0443ed5dbfb9a0428
arm64mac=5301cdf3115f7ada8094024f34e713a803e55de064535cee7e911e2bde496927
arm64win=494e103059a8fc677cd865b1c4d35d444596bcac66c856cd8d94160877b6fc4d
arm64linux=f07929922a4f75f088197f7172efa0384fc72f18560e90be79c17d711aff77b8
arm64musl=f07929922a4f75f088197f7172efa0384fc72f18560e90be79c17d711aff77b8
arm64glibc=f07929922a4f75f088197f7172efa0384fc72f18560e90be79c17d711aff77b8
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
