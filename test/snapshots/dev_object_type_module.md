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
x64mac=0104c97f531ec2fa792aef38ac18f514c5d23acb4ab3114109c92278458e6b93
x64win=c7be2335f84b9b594f5573487b97520113270d34e5d6b12c0024abd6667b21d0
x64mingw=c7be2335f84b9b594f5573487b97520113270d34e5d6b12c0024abd6667b21d0
x64freebsd=d82934e4387e743379c19d8d215395bf679286223d7cf0313eff1ab19c2f6a6c
x64openbsd=b2ed381ad336f8895178c304606cdefcde454745b84d82168032764a990e4677
x64netbsd=67788b5a1c4b81c5d38a9819db5455d8b4e0286104fde43e9ef53e00af967310
x64musl=67788b5a1c4b81c5d38a9819db5455d8b4e0286104fde43e9ef53e00af967310
x64glibc=67788b5a1c4b81c5d38a9819db5455d8b4e0286104fde43e9ef53e00af967310
x64linux=67788b5a1c4b81c5d38a9819db5455d8b4e0286104fde43e9ef53e00af967310
x64elf=67788b5a1c4b81c5d38a9819db5455d8b4e0286104fde43e9ef53e00af967310
x64v1mac=0104c97f531ec2fa792aef38ac18f514c5d23acb4ab3114109c92278458e6b93
x64v1win=c7be2335f84b9b594f5573487b97520113270d34e5d6b12c0024abd6667b21d0
x64v1mingw=c7be2335f84b9b594f5573487b97520113270d34e5d6b12c0024abd6667b21d0
x64v1freebsd=d82934e4387e743379c19d8d215395bf679286223d7cf0313eff1ab19c2f6a6c
x64v1openbsd=b2ed381ad336f8895178c304606cdefcde454745b84d82168032764a990e4677
x64v1netbsd=67788b5a1c4b81c5d38a9819db5455d8b4e0286104fde43e9ef53e00af967310
x64v1musl=67788b5a1c4b81c5d38a9819db5455d8b4e0286104fde43e9ef53e00af967310
x64v1glibc=67788b5a1c4b81c5d38a9819db5455d8b4e0286104fde43e9ef53e00af967310
x64v1linux=67788b5a1c4b81c5d38a9819db5455d8b4e0286104fde43e9ef53e00af967310
x64v1elf=67788b5a1c4b81c5d38a9819db5455d8b4e0286104fde43e9ef53e00af967310
arm64mac=987b325e5c67ad5dd163e35ea394497b8e3b4abcc10257e87c41782f9537a88f
arm64win=6c929a8bbaec5345c620c98c2172216d44c1c4506bc5c99cc8f008142371794b
arm64mingw=6c929a8bbaec5345c620c98c2172216d44c1c4506bc5c99cc8f008142371794b
arm64linux=7c0dddf9951ddb0ef6d7214fa6dabd1ad741de6a0474e765ce862e4cdcb0cc30
arm64musl=7c0dddf9951ddb0ef6d7214fa6dabd1ad741de6a0474e765ce862e4cdcb0cc30
arm64glibc=7c0dddf9951ddb0ef6d7214fa6dabd1ad741de6a0474e765ce862e4cdcb0cc30
arm64v1win=6c929a8bbaec5345c620c98c2172216d44c1c4506bc5c99cc8f008142371794b
arm64v1mingw=6c929a8bbaec5345c620c98c2172216d44c1c4506bc5c99cc8f008142371794b
arm64v1linux=7c0dddf9951ddb0ef6d7214fa6dabd1ad741de6a0474e765ce862e4cdcb0cc30
arm64v1musl=7c0dddf9951ddb0ef6d7214fa6dabd1ad741de6a0474e765ce862e4cdcb0cc30
arm64v1glibc=7c0dddf9951ddb0ef6d7214fa6dabd1ad741de6a0474e765ce862e4cdcb0cc30
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
