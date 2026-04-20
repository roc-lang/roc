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
x64mac=488150ad8a7bfc7c527e20468ebfef92298f21ce2a7542eb2d6a57af5ae56a2e
x64win=977adab69bb70a7747de69715dbd304b7ea18e8698b1cb3caf5bf59d3b630863
x64freebsd=578eaa56ca6209839bad039eac64d7f1bffbf4d98b9a89ed7df44783ce11d1bc
x64openbsd=578eaa56ca6209839bad039eac64d7f1bffbf4d98b9a89ed7df44783ce11d1bc
x64netbsd=578eaa56ca6209839bad039eac64d7f1bffbf4d98b9a89ed7df44783ce11d1bc
x64musl=578eaa56ca6209839bad039eac64d7f1bffbf4d98b9a89ed7df44783ce11d1bc
x64glibc=578eaa56ca6209839bad039eac64d7f1bffbf4d98b9a89ed7df44783ce11d1bc
x64linux=578eaa56ca6209839bad039eac64d7f1bffbf4d98b9a89ed7df44783ce11d1bc
x64elf=578eaa56ca6209839bad039eac64d7f1bffbf4d98b9a89ed7df44783ce11d1bc
arm64mac=67813b4fcc5fe39b0503433243996bf5a139aa091bf17e56f75e6b1318feba4e
arm64win=660672f5127d49f749ec41144f87d6364e16fcae3e584f532c81eeb913c2544d
arm64linux=b1d9247ca95e9b87623eb0b488c2f5fbf2d973c5e339fc9ec073e41028f9a4e7
arm64musl=b1d9247ca95e9b87623eb0b488c2f5fbf2d973c5e339fc9ec073e41028f9a4e7
arm64glibc=b1d9247ca95e9b87623eb0b488c2f5fbf2d973c5e339fc9ec073e41028f9a4e7
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
