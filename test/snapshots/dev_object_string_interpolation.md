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
arm64mac=12a0a8f531e1e35ece881f6a5f4f85ee51ab3cc74909cbf688e50dfa032aed34
arm64win=1ed1f927bb192df02444c3ab8dd42470d1647a5c5a0a7170511b92e3a3bd70e2
arm64linux=87e398bcbcd62b61043d5a462e31fa237e9a358b3359dd626717bbbf09440626
arm64musl=87e398bcbcd62b61043d5a462e31fa237e9a358b3359dd626717bbbf09440626
arm64glibc=87e398bcbcd62b61043d5a462e31fa237e9a358b3359dd626717bbbf09440626
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
