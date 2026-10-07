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
x64mac=4739e896985a2ab7a71fe3b3ff09022676760eccf096aa7a02586c752a8e35e3
x64win=352f747dde6e81c1a4f26cbc6523d59e9bf68d78558efe45036db0a480af6603
x64mingw=352f747dde6e81c1a4f26cbc6523d59e9bf68d78558efe45036db0a480af6603
x64freebsd=3c6e3d85d3d041f00d971e4bd18711febf6c705b5c49426f9d261930f828f067
x64openbsd=9a305ac60798346c2c8eb003a7f90dfc07ff75f5cd0dd294e8fe24eb10b53c97
x64netbsd=898dc144a0a2ad4a373c5413cd0c9b314402571566dad898ff375f80b6af9d8b
x64musl=898dc144a0a2ad4a373c5413cd0c9b314402571566dad898ff375f80b6af9d8b
x64glibc=898dc144a0a2ad4a373c5413cd0c9b314402571566dad898ff375f80b6af9d8b
x64linux=898dc144a0a2ad4a373c5413cd0c9b314402571566dad898ff375f80b6af9d8b
x64elf=898dc144a0a2ad4a373c5413cd0c9b314402571566dad898ff375f80b6af9d8b
x64v1mac=4739e896985a2ab7a71fe3b3ff09022676760eccf096aa7a02586c752a8e35e3
x64v1win=352f747dde6e81c1a4f26cbc6523d59e9bf68d78558efe45036db0a480af6603
x64v1mingw=352f747dde6e81c1a4f26cbc6523d59e9bf68d78558efe45036db0a480af6603
x64v1freebsd=3c6e3d85d3d041f00d971e4bd18711febf6c705b5c49426f9d261930f828f067
x64v1openbsd=9a305ac60798346c2c8eb003a7f90dfc07ff75f5cd0dd294e8fe24eb10b53c97
x64v1netbsd=898dc144a0a2ad4a373c5413cd0c9b314402571566dad898ff375f80b6af9d8b
x64v1musl=898dc144a0a2ad4a373c5413cd0c9b314402571566dad898ff375f80b6af9d8b
x64v1glibc=898dc144a0a2ad4a373c5413cd0c9b314402571566dad898ff375f80b6af9d8b
x64v1linux=898dc144a0a2ad4a373c5413cd0c9b314402571566dad898ff375f80b6af9d8b
x64v1elf=898dc144a0a2ad4a373c5413cd0c9b314402571566dad898ff375f80b6af9d8b
arm64mac=abe2e949a3a3e567e3b00e1b6f6c35f291d535c1daf9363dce18659c9334f966
arm64win=455fe1cd3946dd607e76daa2fa1a2195f766ebcd28ca935330428d7d12377588
arm64mingw=455fe1cd3946dd607e76daa2fa1a2195f766ebcd28ca935330428d7d12377588
arm64linux=9a0cde36ba1294c5f162a93adebb95e54b477c40475e58ca67d70f5bf4b5848b
arm64musl=9a0cde36ba1294c5f162a93adebb95e54b477c40475e58ca67d70f5bf4b5848b
arm64glibc=9a0cde36ba1294c5f162a93adebb95e54b477c40475e58ca67d70f5bf4b5848b
arm64v1win=455fe1cd3946dd607e76daa2fa1a2195f766ebcd28ca935330428d7d12377588
arm64v1mingw=455fe1cd3946dd607e76daa2fa1a2195f766ebcd28ca935330428d7d12377588
arm64v1linux=9a0cde36ba1294c5f162a93adebb95e54b477c40475e58ca67d70f5bf4b5848b
arm64v1musl=9a0cde36ba1294c5f162a93adebb95e54b477c40475e58ca67d70f5bf4b5848b
arm64v1glibc=9a0cde36ba1294c5f162a93adebb95e54b477c40475e58ca67d70f5bf4b5848b
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
