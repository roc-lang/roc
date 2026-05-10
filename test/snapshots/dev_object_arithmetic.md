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
# platform
main_for_host = <required>

# app
main = add(3, 4) * 2
add = |a, b| a + b

~~~
# DEV OUTPUT
~~~ini
x64mac=4953a102309efe7f78bb94052ad469dbafb8e3fe5fbb536174456a6f630e4f89
x64win=72862574fdacefbcd10ee31b471295b24c7f46242ec9e44cab4375cc5172b18b
x64freebsd=87912822472ba2c00fc465f387f449a754e6bc922b5d9d358e171b3d7b1918b9
x64openbsd=87912822472ba2c00fc465f387f449a754e6bc922b5d9d358e171b3d7b1918b9
x64netbsd=87912822472ba2c00fc465f387f449a754e6bc922b5d9d358e171b3d7b1918b9
x64musl=87912822472ba2c00fc465f387f449a754e6bc922b5d9d358e171b3d7b1918b9
x64glibc=87912822472ba2c00fc465f387f449a754e6bc922b5d9d358e171b3d7b1918b9
x64linux=87912822472ba2c00fc465f387f449a754e6bc922b5d9d358e171b3d7b1918b9
x64elf=87912822472ba2c00fc465f387f449a754e6bc922b5d9d358e171b3d7b1918b9
arm64mac=ab547599402823729c6c67be258cec19c6dbcfe48b583cc93e160a8de93de0ab
arm64win=ac7c5a8bcfcc587fa17a37085c2b3388b9b034c670a12de1b81f3a40a6bc3f79
arm64linux=f2ce8d4008b5ff215f24e9ca4e155c7339853fa92e19e5d7a7e6afae752487c1
arm64musl=f2ce8d4008b5ff215f24e9ca4e155c7339853fa92e19e5d7a7e6afae752487c1
arm64glibc=f2ce8d4008b5ff215f24e9ca4e155c7339853fa92e19e5d7a7e6afae752487c1
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
