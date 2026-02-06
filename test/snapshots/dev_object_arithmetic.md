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
x64mac=f26e6e593934c8d00ed022b171435b7a611fcc12051be854cdd00f352615d8a2
x64win=8b17ee5da643b626854d28e37f85f08d121dac1b18270c332699541cca902c73
x64freebsd=fc043d520831da2abd4cef2d1eac6f15020acac35195d04c2b9707c75bdd9bdc
x64openbsd=fc043d520831da2abd4cef2d1eac6f15020acac35195d04c2b9707c75bdd9bdc
x64netbsd=fc043d520831da2abd4cef2d1eac6f15020acac35195d04c2b9707c75bdd9bdc
x64musl=fc043d520831da2abd4cef2d1eac6f15020acac35195d04c2b9707c75bdd9bdc
x64glibc=fc043d520831da2abd4cef2d1eac6f15020acac35195d04c2b9707c75bdd9bdc
x64linux=fc043d520831da2abd4cef2d1eac6f15020acac35195d04c2b9707c75bdd9bdc
x64elf=fc043d520831da2abd4cef2d1eac6f15020acac35195d04c2b9707c75bdd9bdc
arm64mac=9efc20a01931edb8e287b88c87b0d19f3be014695ceb2fc681cbefce18ba7059
arm64win=17c73750e3f90879324ac4ae59f1a932214e6c2b5068846eab3ddb4228350797
arm64linux=d8aa092b6b90807aa8550f9c0ecc8acf6be7a15e7acbd1e6e6c3b4cb84a2a1a6
arm64musl=d8aa092b6b90807aa8550f9c0ecc8acf6be7a15e7acbd1e6e6c3b4cb84a2a1a6
arm64glibc=d8aa092b6b90807aa8550f9c0ecc8acf6be7a15e7acbd1e6e6c3b4cb84a2a1a6
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
