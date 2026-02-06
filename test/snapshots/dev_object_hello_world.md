# META
~~~ini
description=Hello world dev object compilation
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

main = "Hello, World!"
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
main = "Hello, World!"

# platform
main_for_host = <required>

~~~
# DEV OUTPUT
~~~ini
x64mac=f3032d913a81aa83ec91c8bb57a9bf6404f11cac71d46ceb24b2197fc5ebfc88
x64win=e9066cf7d98625cb84f915d124d7e632bdf8b2176c9b4a945b7702dd1015e7af
x64freebsd=d51e27bfae836757524cdfcef8a97f8d7cd95970c66bdd36b7d9a348b7246615
x64openbsd=d51e27bfae836757524cdfcef8a97f8d7cd95970c66bdd36b7d9a348b7246615
x64netbsd=d51e27bfae836757524cdfcef8a97f8d7cd95970c66bdd36b7d9a348b7246615
x64musl=d51e27bfae836757524cdfcef8a97f8d7cd95970c66bdd36b7d9a348b7246615
x64glibc=d51e27bfae836757524cdfcef8a97f8d7cd95970c66bdd36b7d9a348b7246615
x64linux=d51e27bfae836757524cdfcef8a97f8d7cd95970c66bdd36b7d9a348b7246615
x64elf=d51e27bfae836757524cdfcef8a97f8d7cd95970c66bdd36b7d9a348b7246615
arm64mac=20bc4d674dbda738f3ba8ca9263ee15cf9c895a5d317e341403ccc8effa1ae19
arm64win=3c66960c108be0e8ab6f95b2b17601667e1faa31f0b0a158e1d0d3cc941a0aed
arm64linux=dcd0a704d949c11660d92148536c2aa9b815e0b537898bae75c99eb01b5998c7
arm64musl=dcd0a704d949c11660d92148536c2aa9b815e0b537898bae75c99eb01b5998c7
arm64glibc=dcd0a704d949c11660d92148536c2aa9b815e0b537898bae75c99eb01b5998c7
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
