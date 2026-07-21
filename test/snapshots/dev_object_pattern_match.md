# META
~~~ini
description=Tag unions and pattern matching
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

Color : [Red, Green, Blue]

to_str : Color -> Str
to_str = |color|
    match color {
        Red => "red"
        Green => "green"
        Blue => "blue"
    }

main = to_str(Red)
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : Str }
    exposes []
    packages {}
    provides { "roc_main": main_for_host }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
    }

main_for_host : Str
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# app
to_str = |color| match color {
	Red => "red"
	Green => "green"
	Blue => "blue"
}
main = to_str(Red)

~~~
# DEV OUTPUT
~~~ini
x64mac=f7bc6d7588300a422ece240a5a52ca7a964bfe93bc499b4aa6ea30eefc7dbc4a
x64win=ad1650cf281a87d60d07117e2cc42b17d512412a69fbcc2fed3b7ece6852c0e9
x64freebsd=8335222e5d834939ce28e8f13ea460186d72cf703c000f7422076a15eb8fa163
x64openbsd=8335222e5d834939ce28e8f13ea460186d72cf703c000f7422076a15eb8fa163
x64netbsd=8335222e5d834939ce28e8f13ea460186d72cf703c000f7422076a15eb8fa163
x64musl=8335222e5d834939ce28e8f13ea460186d72cf703c000f7422076a15eb8fa163
x64glibc=8335222e5d834939ce28e8f13ea460186d72cf703c000f7422076a15eb8fa163
x64linux=8335222e5d834939ce28e8f13ea460186d72cf703c000f7422076a15eb8fa163
x64elf=8335222e5d834939ce28e8f13ea460186d72cf703c000f7422076a15eb8fa163
arm64mac=91aa77df7234a7058d38f0a27c5afe7da103f55beb57b79f71a2976a7a3ad763
arm64win=9fe5192b3d873234c5a6e0a27a66a3769bafcaaeac1cdd6df2c355639a5de0b6
arm64linux=dde2b6d483c5db19c8f5e46b8eba8770fed1872f8d16f62332969cb3c27e65ad
arm64musl=dde2b6d483c5db19c8f5e46b8eba8770fed1872f8d16f62332969cb3c27e65ad
arm64glibc=dde2b6d483c5db19c8f5e46b8eba8770fed1872f8d16f62332969cb3c27e65ad
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
