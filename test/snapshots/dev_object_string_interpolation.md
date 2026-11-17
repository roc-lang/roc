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
greeting = "Hello"
name = "World"
main = {
	cinterp_0 = greeting
	cinterp_1 = name
	<interpolation>("", [cinterp_0, ", ", cinterp_1, "!"])
}

~~~
# DEV OUTPUT
~~~ini
x64mac=5a30fe809a01a353b51e905e316b4f3a28d84f1be44684612b828c26493e585d
x64win=6db7a928d6eb693913cac57a0a43e4f9dc94f635fc3312d6e89b94966dc4d45c
x64mingw=6db7a928d6eb693913cac57a0a43e4f9dc94f635fc3312d6e89b94966dc4d45c
x64freebsd=07c5e0c7bcebc4d1a4b745d9cb3d0094ef87135518714e132a8c6a5b1866df1a
x64openbsd=44ffe2553b8e770447e1f0683de18f108f2dc53a76912f8365b9892d54032d44
x64netbsd=a524b5666a9623c138c8ef8cb24d07debe875bb0cb10a526b9014725067db24c
x64musl=a524b5666a9623c138c8ef8cb24d07debe875bb0cb10a526b9014725067db24c
x64glibc=a524b5666a9623c138c8ef8cb24d07debe875bb0cb10a526b9014725067db24c
x64linux=a524b5666a9623c138c8ef8cb24d07debe875bb0cb10a526b9014725067db24c
x64elf=a524b5666a9623c138c8ef8cb24d07debe875bb0cb10a526b9014725067db24c
x64v1mac=5a30fe809a01a353b51e905e316b4f3a28d84f1be44684612b828c26493e585d
x64v1win=6db7a928d6eb693913cac57a0a43e4f9dc94f635fc3312d6e89b94966dc4d45c
x64v1mingw=6db7a928d6eb693913cac57a0a43e4f9dc94f635fc3312d6e89b94966dc4d45c
x64v1freebsd=07c5e0c7bcebc4d1a4b745d9cb3d0094ef87135518714e132a8c6a5b1866df1a
x64v1openbsd=44ffe2553b8e770447e1f0683de18f108f2dc53a76912f8365b9892d54032d44
x64v1netbsd=a524b5666a9623c138c8ef8cb24d07debe875bb0cb10a526b9014725067db24c
x64v1musl=a524b5666a9623c138c8ef8cb24d07debe875bb0cb10a526b9014725067db24c
x64v1glibc=a524b5666a9623c138c8ef8cb24d07debe875bb0cb10a526b9014725067db24c
x64v1linux=a524b5666a9623c138c8ef8cb24d07debe875bb0cb10a526b9014725067db24c
x64v1elf=a524b5666a9623c138c8ef8cb24d07debe875bb0cb10a526b9014725067db24c
arm64mac=8478b5eb5de989895bec4d942e80ec8ec46a62f3d2be4fbf508c06ddf698b64f
arm64win=e58d581d82b834dc516c0de2c5e72296ec69a8fccc3506bf6878b9edf50db585
arm64mingw=e58d581d82b834dc516c0de2c5e72296ec69a8fccc3506bf6878b9edf50db585
arm64linux=989e0d3786605a0adef2fd9683759a7c002fd635f4b1b73a2f26f7ffafacb4c7
arm64musl=989e0d3786605a0adef2fd9683759a7c002fd635f4b1b73a2f26f7ffafacb4c7
arm64glibc=989e0d3786605a0adef2fd9683759a7c002fd635f4b1b73a2f26f7ffafacb4c7
arm64v1win=e58d581d82b834dc516c0de2c5e72296ec69a8fccc3506bf6878b9edf50db585
arm64v1mingw=e58d581d82b834dc516c0de2c5e72296ec69a8fccc3506bf6878b9edf50db585
arm64v1linux=989e0d3786605a0adef2fd9683759a7c002fd635f4b1b73a2f26f7ffafacb4c7
arm64v1musl=989e0d3786605a0adef2fd9683759a7c002fd635f4b1b73a2f26f7ffafacb4c7
arm64v1glibc=989e0d3786605a0adef2fd9683759a7c002fd635f4b1b73a2f26f7ffafacb4c7
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
