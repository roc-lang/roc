# META
~~~ini
description=Type mod import with multi-mod compilation
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

import Color

main = Color.to_str(Color.red({}))
~~~
## Color.roc
~~~roc
Color := [Red, Green, Blue].{
    red : {} -> Color
    red = |{}| Red

    green : {} -> Color
    green = |{}| Green

    blue : {} -> Color
    blue = |{}| Blue

    to_str : Color -> Str
    to_str = |color|
        match color {
            Red => "red"
            Green => "green"
            Blue => "blue"
        }
}
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

# Color
Color.red = |{}| Red
Color.green = |{}| Green
Color.blue = |{}| Blue
Color.to_str = |color| match color {
	Red => "red"
	Green => "green"
	Blue => "blue"
}

# app
main = to_str(red({}))

~~~
# DEV OUTPUT
~~~ini
x64mac=379b30a4270afb9dcaadb375935d4cb86e6d90e1750eadd29f64e68bf1fa4158
x64win=04d29351bf4fb6634674afda87e9e5c8401bd41b27334506deb8c42b09ece6d8
x64mingw=04d29351bf4fb6634674afda87e9e5c8401bd41b27334506deb8c42b09ece6d8
x64freebsd=0b48ad09c7d1459852afdab5b62b979f2b610d13e64d9084f16e1f1b7bd6c588
x64openbsd=d3b3302aefbe4169a18d7ee09776e68d7f3439778f5fb8b9034338b390ecb17c
x64netbsd=d1b4e77492b9603b85a49187b63866ef45077104c46feb4010627a763ed494b2
x64musl=d1b4e77492b9603b85a49187b63866ef45077104c46feb4010627a763ed494b2
x64glibc=d1b4e77492b9603b85a49187b63866ef45077104c46feb4010627a763ed494b2
x64linux=d1b4e77492b9603b85a49187b63866ef45077104c46feb4010627a763ed494b2
x64elf=d1b4e77492b9603b85a49187b63866ef45077104c46feb4010627a763ed494b2
x64v1mac=379b30a4270afb9dcaadb375935d4cb86e6d90e1750eadd29f64e68bf1fa4158
x64v1win=04d29351bf4fb6634674afda87e9e5c8401bd41b27334506deb8c42b09ece6d8
x64v1mingw=04d29351bf4fb6634674afda87e9e5c8401bd41b27334506deb8c42b09ece6d8
x64v1freebsd=0b48ad09c7d1459852afdab5b62b979f2b610d13e64d9084f16e1f1b7bd6c588
x64v1openbsd=d3b3302aefbe4169a18d7ee09776e68d7f3439778f5fb8b9034338b390ecb17c
x64v1netbsd=d1b4e77492b9603b85a49187b63866ef45077104c46feb4010627a763ed494b2
x64v1musl=d1b4e77492b9603b85a49187b63866ef45077104c46feb4010627a763ed494b2
x64v1glibc=d1b4e77492b9603b85a49187b63866ef45077104c46feb4010627a763ed494b2
x64v1linux=d1b4e77492b9603b85a49187b63866ef45077104c46feb4010627a763ed494b2
x64v1elf=d1b4e77492b9603b85a49187b63866ef45077104c46feb4010627a763ed494b2
arm64mac=6f45ad51a438c9a6a03be1fef3d738956f42de44c10b5feaa3c6b907c2aaa71c
arm64win=685c9dca6432b3ff3344cbd43a10a277a21219db64db7dcac6ae0390cd3452a2
arm64mingw=685c9dca6432b3ff3344cbd43a10a277a21219db64db7dcac6ae0390cd3452a2
arm64linux=902e8c93460351a9476430a7a1117b9e88785aabdc616f23f57abb2616aebdfd
arm64musl=902e8c93460351a9476430a7a1117b9e88785aabdc616f23f57abb2616aebdfd
arm64glibc=902e8c93460351a9476430a7a1117b9e88785aabdc616f23f57abb2616aebdfd
arm64v1win=685c9dca6432b3ff3344cbd43a10a277a21219db64db7dcac6ae0390cd3452a2
arm64v1mingw=685c9dca6432b3ff3344cbd43a10a277a21219db64db7dcac6ae0390cd3452a2
arm64v1linux=902e8c93460351a9476430a7a1117b9e88785aabdc616f23f57abb2616aebdfd
arm64v1musl=902e8c93460351a9476430a7a1117b9e88785aabdc616f23f57abb2616aebdfd
arm64v1glibc=902e8c93460351a9476430a7a1117b9e88785aabdc616f23f57abb2616aebdfd
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
