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
x64mac=087c2c55a4c9f7b1925cf79e0b2c5033f083fea3870977a059a023a19d8c2d3e
x64win=ad2e48d536e37a1391aa5065dc473a8b6358d6e2a4f74e0e92affb3ce70b8c45
x64freebsd=f2fc74c2f1b0a0772aae95b7e424602272d010d48ff8da14cfc7cfe2ffc2f8d3
x64openbsd=f2fc74c2f1b0a0772aae95b7e424602272d010d48ff8da14cfc7cfe2ffc2f8d3
x64netbsd=f2fc74c2f1b0a0772aae95b7e424602272d010d48ff8da14cfc7cfe2ffc2f8d3
x64musl=f2fc74c2f1b0a0772aae95b7e424602272d010d48ff8da14cfc7cfe2ffc2f8d3
x64glibc=f2fc74c2f1b0a0772aae95b7e424602272d010d48ff8da14cfc7cfe2ffc2f8d3
x64linux=f2fc74c2f1b0a0772aae95b7e424602272d010d48ff8da14cfc7cfe2ffc2f8d3
x64elf=f2fc74c2f1b0a0772aae95b7e424602272d010d48ff8da14cfc7cfe2ffc2f8d3
arm64mac=9efc20a01931edb8e287b88c87b0d19f3be014695ceb2fc681cbefce18ba7059
arm64win=17c73750e3f90879324ac4ae59f1a932214e6c2b5068846eab3ddb4228350797
arm64linux=d8aa092b6b90807aa8550f9c0ecc8acf6be7a15e7acbd1e6e6c3b4cb84a2a1a6
arm64musl=d8aa092b6b90807aa8550f9c0ecc8acf6be7a15e7acbd1e6e6c3b4cb84a2a1a6
arm64glibc=d8aa092b6b90807aa8550f9c0ecc8acf6be7a15e7acbd1e6e6c3b4cb84a2a1a6
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
