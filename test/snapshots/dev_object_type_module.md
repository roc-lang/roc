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
x64mac=a0a41c825eae6f10ad6d900809938305d343b92e6192ddaa618aa4b774459210
x64win=9e004f4cd65a81026c20821f3160b370348286b70db0e74f7f8795b406462e93
x64mingw=9e004f4cd65a81026c20821f3160b370348286b70db0e74f7f8795b406462e93
x64freebsd=695a453e006e8de4182ab8109e500f55e72d2acbe0492971244dc22e2d66bad2
x64openbsd=7631e6102e9d65346f09905fce7e3d481b14bf87a9d910d42941edba3865ef30
x64netbsd=52a2aea2cc52a9f7e863369d22c4f82946be503119d10089c219f2d6d87a0b49
x64musl=52a2aea2cc52a9f7e863369d22c4f82946be503119d10089c219f2d6d87a0b49
x64glibc=52a2aea2cc52a9f7e863369d22c4f82946be503119d10089c219f2d6d87a0b49
x64linux=52a2aea2cc52a9f7e863369d22c4f82946be503119d10089c219f2d6d87a0b49
x64elf=52a2aea2cc52a9f7e863369d22c4f82946be503119d10089c219f2d6d87a0b49
x64v1mac=a0a41c825eae6f10ad6d900809938305d343b92e6192ddaa618aa4b774459210
x64v1win=9e004f4cd65a81026c20821f3160b370348286b70db0e74f7f8795b406462e93
x64v1mingw=9e004f4cd65a81026c20821f3160b370348286b70db0e74f7f8795b406462e93
x64v1freebsd=695a453e006e8de4182ab8109e500f55e72d2acbe0492971244dc22e2d66bad2
x64v1openbsd=7631e6102e9d65346f09905fce7e3d481b14bf87a9d910d42941edba3865ef30
x64v1netbsd=52a2aea2cc52a9f7e863369d22c4f82946be503119d10089c219f2d6d87a0b49
x64v1musl=52a2aea2cc52a9f7e863369d22c4f82946be503119d10089c219f2d6d87a0b49
x64v1glibc=52a2aea2cc52a9f7e863369d22c4f82946be503119d10089c219f2d6d87a0b49
x64v1linux=52a2aea2cc52a9f7e863369d22c4f82946be503119d10089c219f2d6d87a0b49
x64v1elf=52a2aea2cc52a9f7e863369d22c4f82946be503119d10089c219f2d6d87a0b49
arm64mac=258140811bf2c4a41d910983c7479e98303437b5023ec24a7a0c18e5384121bd
arm64win=3235c1d49027b36a011941306948007b47a257e4752f32273ae72fa412624f31
arm64mingw=3235c1d49027b36a011941306948007b47a257e4752f32273ae72fa412624f31
arm64linux=5516d0b7e9e79c0d9612e7ea2891f8195da78685c89f43a0fde96eebbb3c6a49
arm64musl=5516d0b7e9e79c0d9612e7ea2891f8195da78685c89f43a0fde96eebbb3c6a49
arm64glibc=5516d0b7e9e79c0d9612e7ea2891f8195da78685c89f43a0fde96eebbb3c6a49
arm64v1win=3235c1d49027b36a011941306948007b47a257e4752f32273ae72fa412624f31
arm64v1mingw=3235c1d49027b36a011941306948007b47a257e4752f32273ae72fa412624f31
arm64v1linux=5516d0b7e9e79c0d9612e7ea2891f8195da78685c89f43a0fde96eebbb3c6a49
arm64v1musl=5516d0b7e9e79c0d9612e7ea2891f8195da78685c89f43a0fde96eebbb3c6a49
arm64v1glibc=5516d0b7e9e79c0d9612e7ea2891f8195da78685c89f43a0fde96eebbb3c6a49
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
