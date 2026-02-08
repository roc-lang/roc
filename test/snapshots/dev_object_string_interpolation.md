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
x64mac=058d3fd800914bb3b6cd681bb768378d1bae4cc6e19694bf9aa0bb1db061dedb
x64win=a38f6bec5f6817a0847ba6119fd98042413d81b8bee17a290989b7739906efdc
x64freebsd=69f569b30c22ab47088cffab49d615439118c7c8414d385b69e6fb74bef769a3
x64openbsd=69f569b30c22ab47088cffab49d615439118c7c8414d385b69e6fb74bef769a3
x64netbsd=69f569b30c22ab47088cffab49d615439118c7c8414d385b69e6fb74bef769a3
x64musl=69f569b30c22ab47088cffab49d615439118c7c8414d385b69e6fb74bef769a3
x64glibc=69f569b30c22ab47088cffab49d615439118c7c8414d385b69e6fb74bef769a3
x64linux=69f569b30c22ab47088cffab49d615439118c7c8414d385b69e6fb74bef769a3
x64elf=69f569b30c22ab47088cffab49d615439118c7c8414d385b69e6fb74bef769a3
arm64mac=01b7ddf98e09218be93e4c80edcb296dbdb7ddb0a3eff02f5c37cb09bf9e16ff
arm64win=5ae3d9c7da096d50e45b6576ee121f9693cb3e74263072e2e6b01df3a65abe21
arm64linux=fb0f93b6bd89d86b76f40fb2f5458fd3199cdebedc3de7ad8d651f1bd83ee014
arm64musl=fb0f93b6bd89d86b76f40fb2f5458fd3199cdebedc3de7ad8d651f1bd83ee014
arm64glibc=fb0f93b6bd89d86b76f40fb2f5458fd3199cdebedc3de7ad8d651f1bd83ee014
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
