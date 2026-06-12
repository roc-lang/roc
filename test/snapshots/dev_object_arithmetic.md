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
        inputs: "targets/",
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
main = add(3, 4).times(2)
add = |a, b| a.plus(b)

~~~
# DEV OUTPUT
~~~ini
x64mac=086e70cd81ae939b78b78f3bab72cf2bceecc84c5f79ba4eff63edcd67f21ab4
x64win=d286ad7c93561a310b64656d24d94ef6947b69f04b0fa9d5d9f9821561861022
x64freebsd=fa50219ac72ce3b35f414a3fdf3b9caa2f29cc17a6f0c2c8bb2e522da549bfdb
x64openbsd=fa50219ac72ce3b35f414a3fdf3b9caa2f29cc17a6f0c2c8bb2e522da549bfdb
x64netbsd=fa50219ac72ce3b35f414a3fdf3b9caa2f29cc17a6f0c2c8bb2e522da549bfdb
x64musl=fa50219ac72ce3b35f414a3fdf3b9caa2f29cc17a6f0c2c8bb2e522da549bfdb
x64glibc=fa50219ac72ce3b35f414a3fdf3b9caa2f29cc17a6f0c2c8bb2e522da549bfdb
x64linux=fa50219ac72ce3b35f414a3fdf3b9caa2f29cc17a6f0c2c8bb2e522da549bfdb
x64elf=fa50219ac72ce3b35f414a3fdf3b9caa2f29cc17a6f0c2c8bb2e522da549bfdb
arm64mac=660b31a68da57ef24bf1a168bbc7fc2afe7fef4f5eacea395c34337e51bd98e6
arm64win=00b03a5f9c21f616abad611ed8755c45f7338556759adbfdb38c8ede7b0c61cb
arm64linux=59ddedce89946ebf4a1ed88a340c1028dd626af90cb659d7852c6321b96d69e6
arm64musl=59ddedce89946ebf4a1ed88a340c1028dd626af90cb659d7852c6321b96d69e6
arm64glibc=59ddedce89946ebf4a1ed88a340c1028dd626af90cb659d7852c6321b96d69e6
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
