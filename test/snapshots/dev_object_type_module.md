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
x64mac=c447a024155382463106a132292913093b86a5ea6d07b1c85c4e83092f072e29
x64win=13fc62a75ea06033464e4996e5c1c0f3c1472238331f188add183ba907f4874f
x64mingw=13fc62a75ea06033464e4996e5c1c0f3c1472238331f188add183ba907f4874f
x64freebsd=ea514b349ba5f9b6e2d84be66e49a063a98f9270bee319aaf56793c8d6c8c898
x64openbsd=2b0a005ceb41712b6b1b5d0414da4e5b0b5bb8ac654b669687af3f4d561e676b
x64netbsd=798d864e9d3cfeb03f6d8e0106d859672da8c9973e75ade4162d795ed4071cad
x64musl=798d864e9d3cfeb03f6d8e0106d859672da8c9973e75ade4162d795ed4071cad
x64glibc=798d864e9d3cfeb03f6d8e0106d859672da8c9973e75ade4162d795ed4071cad
x64linux=798d864e9d3cfeb03f6d8e0106d859672da8c9973e75ade4162d795ed4071cad
x64elf=798d864e9d3cfeb03f6d8e0106d859672da8c9973e75ade4162d795ed4071cad
x64v1mac=c447a024155382463106a132292913093b86a5ea6d07b1c85c4e83092f072e29
x64v1win=13fc62a75ea06033464e4996e5c1c0f3c1472238331f188add183ba907f4874f
x64v1mingw=13fc62a75ea06033464e4996e5c1c0f3c1472238331f188add183ba907f4874f
x64v1freebsd=ea514b349ba5f9b6e2d84be66e49a063a98f9270bee319aaf56793c8d6c8c898
x64v1openbsd=2b0a005ceb41712b6b1b5d0414da4e5b0b5bb8ac654b669687af3f4d561e676b
x64v1netbsd=798d864e9d3cfeb03f6d8e0106d859672da8c9973e75ade4162d795ed4071cad
x64v1musl=798d864e9d3cfeb03f6d8e0106d859672da8c9973e75ade4162d795ed4071cad
x64v1glibc=798d864e9d3cfeb03f6d8e0106d859672da8c9973e75ade4162d795ed4071cad
x64v1linux=798d864e9d3cfeb03f6d8e0106d859672da8c9973e75ade4162d795ed4071cad
x64v1elf=798d864e9d3cfeb03f6d8e0106d859672da8c9973e75ade4162d795ed4071cad
arm64mac=4b5cb843555ab6174f8938e20d48566b4c53b49c9936efd9927debda8f3af8a2
arm64win=3e680c44611272f6b25b05bb127c83f518a7ea0354f9b502cdff3da625f0aaac
arm64mingw=3e680c44611272f6b25b05bb127c83f518a7ea0354f9b502cdff3da625f0aaac
arm64linux=56792268846d0f8b1f17d71cec4e88c3e098a2c82f0fcc5c1847d3aabf291855
arm64musl=56792268846d0f8b1f17d71cec4e88c3e098a2c82f0fcc5c1847d3aabf291855
arm64glibc=56792268846d0f8b1f17d71cec4e88c3e098a2c82f0fcc5c1847d3aabf291855
arm64v1win=3e680c44611272f6b25b05bb127c83f518a7ea0354f9b502cdff3da625f0aaac
arm64v1mingw=3e680c44611272f6b25b05bb127c83f518a7ea0354f9b502cdff3da625f0aaac
arm64v1linux=56792268846d0f8b1f17d71cec4e88c3e098a2c82f0fcc5c1847d3aabf291855
arm64v1musl=56792268846d0f8b1f17d71cec4e88c3e098a2c82f0fcc5c1847d3aabf291855
arm64v1glibc=56792268846d0f8b1f17d71cec4e88c3e098a2c82f0fcc5c1847d3aabf291855
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
