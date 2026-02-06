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
arm64mac=355302e7f40504de3cdab36b5c92f370958ca6629f5655cc4642e6594fda6e9a
arm64win=adc6f0883f396b0f59bb4b4c3c4666cd319df94f6a6f0641b18cbc732eb5cc27
arm64linux=958db6aad587af20a1acd916280b3d353df8d8b30d1e95f746b255b3cf7a3ac3
arm64musl=958db6aad587af20a1acd916280b3d353df8d8b30d1e95f746b255b3cf7a3ac3
arm64glibc=958db6aad587af20a1acd916280b3d353df8d8b30d1e95f746b255b3cf7a3ac3
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
