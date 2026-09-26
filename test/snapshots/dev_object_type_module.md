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
x64mac=b8f923fa58cc65e2e3ea28ea93fecbd44ec965999afecd2d6eaf5648a807004c
x64win=352f747dde6e81c1a4f26cbc6523d59e9bf68d78558efe45036db0a480af6603
x64mingw=352f747dde6e81c1a4f26cbc6523d59e9bf68d78558efe45036db0a480af6603
x64freebsd=9bf3c5264a983e8fa33dcf3e7f0a8fdc7f51740cbe9cf117f5c7385bc4e4b916
x64openbsd=31cc4938aa5947af88c0306ed6b6f917ecc5321b1660365fd00b1d61d4c82207
x64netbsd=76254870448e9409ad214d88bad33d5349d8d26d0819f4143fe59c86ebabac97
x64musl=76254870448e9409ad214d88bad33d5349d8d26d0819f4143fe59c86ebabac97
x64glibc=76254870448e9409ad214d88bad33d5349d8d26d0819f4143fe59c86ebabac97
x64linux=76254870448e9409ad214d88bad33d5349d8d26d0819f4143fe59c86ebabac97
x64elf=76254870448e9409ad214d88bad33d5349d8d26d0819f4143fe59c86ebabac97
x64v1mac=b8f923fa58cc65e2e3ea28ea93fecbd44ec965999afecd2d6eaf5648a807004c
x64v1win=352f747dde6e81c1a4f26cbc6523d59e9bf68d78558efe45036db0a480af6603
x64v1mingw=352f747dde6e81c1a4f26cbc6523d59e9bf68d78558efe45036db0a480af6603
x64v1freebsd=9bf3c5264a983e8fa33dcf3e7f0a8fdc7f51740cbe9cf117f5c7385bc4e4b916
x64v1openbsd=31cc4938aa5947af88c0306ed6b6f917ecc5321b1660365fd00b1d61d4c82207
x64v1netbsd=76254870448e9409ad214d88bad33d5349d8d26d0819f4143fe59c86ebabac97
x64v1musl=76254870448e9409ad214d88bad33d5349d8d26d0819f4143fe59c86ebabac97
x64v1glibc=76254870448e9409ad214d88bad33d5349d8d26d0819f4143fe59c86ebabac97
x64v1linux=76254870448e9409ad214d88bad33d5349d8d26d0819f4143fe59c86ebabac97
x64v1elf=76254870448e9409ad214d88bad33d5349d8d26d0819f4143fe59c86ebabac97
arm64mac=ef39edf0247c90fbe81a737daba3a2cd4aed7d74716678c81226de25eca8718c
arm64win=455fe1cd3946dd607e76daa2fa1a2195f766ebcd28ca935330428d7d12377588
arm64mingw=455fe1cd3946dd607e76daa2fa1a2195f766ebcd28ca935330428d7d12377588
arm64linux=c290f11760dea7dcf8c1569422bd96540f25dc20b12f21be97227dc3598211a5
arm64musl=c290f11760dea7dcf8c1569422bd96540f25dc20b12f21be97227dc3598211a5
arm64glibc=c290f11760dea7dcf8c1569422bd96540f25dc20b12f21be97227dc3598211a5
arm64v1win=455fe1cd3946dd607e76daa2fa1a2195f766ebcd28ca935330428d7d12377588
arm64v1mingw=455fe1cd3946dd607e76daa2fa1a2195f766ebcd28ca935330428d7d12377588
arm64v1linux=c290f11760dea7dcf8c1569422bd96540f25dc20b12f21be97227dc3598211a5
arm64v1musl=c290f11760dea7dcf8c1569422bd96540f25dc20b12f21be97227dc3598211a5
arm64v1glibc=c290f11760dea7dcf8c1569422bd96540f25dc20b12f21be97227dc3598211a5
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
