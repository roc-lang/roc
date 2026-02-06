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
x64mac=3362926b022d353cceaaaf2ed8062ccc22e58ae65b97655f347cad4ba01e10a0
x64win=62fe648943e8ba8eba1ddb98676dfda91412ba2d77f577f575acf230fc8158a3
x64freebsd=b363c46a2083336f478e03966ebf4cc1caa7c2cc63929af956c39da79ec3636d
x64openbsd=b363c46a2083336f478e03966ebf4cc1caa7c2cc63929af956c39da79ec3636d
x64netbsd=b363c46a2083336f478e03966ebf4cc1caa7c2cc63929af956c39da79ec3636d
x64musl=b363c46a2083336f478e03966ebf4cc1caa7c2cc63929af956c39da79ec3636d
x64glibc=b363c46a2083336f478e03966ebf4cc1caa7c2cc63929af956c39da79ec3636d
x64linux=b363c46a2083336f478e03966ebf4cc1caa7c2cc63929af956c39da79ec3636d
x64elf=b363c46a2083336f478e03966ebf4cc1caa7c2cc63929af956c39da79ec3636d
arm64mac=20bc4d674dbda738f3ba8ca9263ee15cf9c895a5d317e341403ccc8effa1ae19
arm64win=3c66960c108be0e8ab6f95b2b17601667e1faa31f0b0a158e1d0d3cc941a0aed
arm64linux=dcd0a704d949c11660d92148536c2aa9b815e0b537898bae75c99eb01b5998c7
arm64musl=dcd0a704d949c11660d92148536c2aa9b815e0b537898bae75c99eb01b5998c7
arm64glibc=dcd0a704d949c11660d92148536c2aa9b815e0b537898bae75c99eb01b5998c7
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
