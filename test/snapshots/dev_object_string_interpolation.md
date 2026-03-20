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
x64mac=fc28cab1f256b385af9b22e504eacbc16470e70b4b4e0c753c895d974373177b
x64win=7ffaf0bcb4ab4ac246b8407fed236282fdf9dfe11c5cea4b8a253b363e0fca78
x64freebsd=168d3f0bbe6710522f57c9ac6efe42169f8f3756f4a08ababc95042d969b0f38
x64openbsd=168d3f0bbe6710522f57c9ac6efe42169f8f3756f4a08ababc95042d969b0f38
x64netbsd=168d3f0bbe6710522f57c9ac6efe42169f8f3756f4a08ababc95042d969b0f38
x64musl=168d3f0bbe6710522f57c9ac6efe42169f8f3756f4a08ababc95042d969b0f38
x64glibc=168d3f0bbe6710522f57c9ac6efe42169f8f3756f4a08ababc95042d969b0f38
x64linux=168d3f0bbe6710522f57c9ac6efe42169f8f3756f4a08ababc95042d969b0f38
x64elf=168d3f0bbe6710522f57c9ac6efe42169f8f3756f4a08ababc95042d969b0f38
arm64mac=05192279f481aba67bdd629b72f67e82577b0f42d094d0087da26029a9bc9adc
arm64win=92126531c9c1eb7cc3ff88b3f0d7aea58997402568927ca29fb4868de52e0422
arm64linux=b4ddbe0b7ad3b960959a2a67632d2a371d5616a3e9b967821510f72f56ed2811
arm64musl=b4ddbe0b7ad3b960959a2a67632d2a371d5616a3e9b967821510f72f56ed2811
arm64glibc=b4ddbe0b7ad3b960959a2a67632d2a371d5616a3e9b967821510f72f56ed2811
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
