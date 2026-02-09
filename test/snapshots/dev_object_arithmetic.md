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
x64mac=a0bbe05fc38302e927cfe755cac1abc66461f1eccb7c283e827f1d35c79c04fe
x64win=568ce81af8f678d1b8e9c051a9f93902bdd52646b1583764e89005754b6f1001
x64freebsd=67a75e4f6cde9382f530fd65f3bc7878f8ca2cb298a41962b9f87204c27d0b9e
x64openbsd=67a75e4f6cde9382f530fd65f3bc7878f8ca2cb298a41962b9f87204c27d0b9e
x64netbsd=67a75e4f6cde9382f530fd65f3bc7878f8ca2cb298a41962b9f87204c27d0b9e
x64musl=67a75e4f6cde9382f530fd65f3bc7878f8ca2cb298a41962b9f87204c27d0b9e
x64glibc=67a75e4f6cde9382f530fd65f3bc7878f8ca2cb298a41962b9f87204c27d0b9e
x64linux=67a75e4f6cde9382f530fd65f3bc7878f8ca2cb298a41962b9f87204c27d0b9e
x64elf=67a75e4f6cde9382f530fd65f3bc7878f8ca2cb298a41962b9f87204c27d0b9e
arm64mac=a532b71d67e47fcc797f94cd59f9830c07fda9218b7e62acb8ce42957ae6437d
arm64win=f12a7f688f976a487045d80fa25a3e3f6d2734b54888995a16aed25bc4c1b2e4
arm64linux=1cfa84e36d1b69f393c7dc1c807ab2ca545302edf4c2c7f1b030bff8912f5bbf
arm64musl=1cfa84e36d1b69f393c7dc1c807ab2ca545302edf4c2c7f1b030bff8912f5bbf
arm64glibc=1cfa84e36d1b69f393c7dc1c807ab2ca545302edf4c2c7f1b030bff8912f5bbf
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
