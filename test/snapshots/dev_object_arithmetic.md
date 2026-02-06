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
arm64mac=a532b71d67e47fcc797f94cd59f9830c07fda9218b7e62acb8ce42957ae6437d
arm64win=f12a7f688f976a487045d80fa25a3e3f6d2734b54888995a16aed25bc4c1b2e4
arm64linux=1cfa84e36d1b69f393c7dc1c807ab2ca545302edf4c2c7f1b030bff8912f5bbf
arm64musl=1cfa84e36d1b69f393c7dc1c807ab2ca545302edf4c2c7f1b030bff8912f5bbf
arm64glibc=1cfa84e36d1b69f393c7dc1c807ab2ca545302edf4c2c7f1b030bff8912f5bbf
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
