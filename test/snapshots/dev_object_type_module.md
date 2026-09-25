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
x64mac=dbca8ebb3e8917c3a1084509c34d625e438d0499b277a0d712b482ff1fc8dbba
x64win=f862c341ec9fa5b212c06c1a11c1891a4b9375df0e409eed08b8bb90b80d59e0
x64mingw=f862c341ec9fa5b212c06c1a11c1891a4b9375df0e409eed08b8bb90b80d59e0
x64freebsd=8972e686d0161d1f00d46ef76157a4dc9ade2e50c21c3f2727d2a54d4d105ef8
x64openbsd=56bbf389ccfc2298864603698cc5384032824627c56b6f9b9f7f2ae1ae098e80
x64netbsd=40b8055469aa5dc6596c0cf3db2f8da28ddc1c4803c7785182ae3f70e013778d
x64musl=40b8055469aa5dc6596c0cf3db2f8da28ddc1c4803c7785182ae3f70e013778d
x64glibc=40b8055469aa5dc6596c0cf3db2f8da28ddc1c4803c7785182ae3f70e013778d
x64linux=40b8055469aa5dc6596c0cf3db2f8da28ddc1c4803c7785182ae3f70e013778d
x64elf=40b8055469aa5dc6596c0cf3db2f8da28ddc1c4803c7785182ae3f70e013778d
x64v1mac=dbca8ebb3e8917c3a1084509c34d625e438d0499b277a0d712b482ff1fc8dbba
x64v1win=f862c341ec9fa5b212c06c1a11c1891a4b9375df0e409eed08b8bb90b80d59e0
x64v1mingw=f862c341ec9fa5b212c06c1a11c1891a4b9375df0e409eed08b8bb90b80d59e0
x64v1freebsd=8972e686d0161d1f00d46ef76157a4dc9ade2e50c21c3f2727d2a54d4d105ef8
x64v1openbsd=56bbf389ccfc2298864603698cc5384032824627c56b6f9b9f7f2ae1ae098e80
x64v1netbsd=40b8055469aa5dc6596c0cf3db2f8da28ddc1c4803c7785182ae3f70e013778d
x64v1musl=40b8055469aa5dc6596c0cf3db2f8da28ddc1c4803c7785182ae3f70e013778d
x64v1glibc=40b8055469aa5dc6596c0cf3db2f8da28ddc1c4803c7785182ae3f70e013778d
x64v1linux=40b8055469aa5dc6596c0cf3db2f8da28ddc1c4803c7785182ae3f70e013778d
x64v1elf=40b8055469aa5dc6596c0cf3db2f8da28ddc1c4803c7785182ae3f70e013778d
arm64mac=532bd5eef23b42c9485755129532d0bd03ba59380dd6c44ba55d195a641f5da2
arm64win=11361df6fd2404679d91270e63c3f947b3917a78d08446c9fa818aa0bf634037
arm64mingw=11361df6fd2404679d91270e63c3f947b3917a78d08446c9fa818aa0bf634037
arm64linux=a119c2f1aa2e8e442a16b74d1bec04090228a5d7ccde03dbc060b78cfd3b5c17
arm64musl=a119c2f1aa2e8e442a16b74d1bec04090228a5d7ccde03dbc060b78cfd3b5c17
arm64glibc=a119c2f1aa2e8e442a16b74d1bec04090228a5d7ccde03dbc060b78cfd3b5c17
arm64v1win=11361df6fd2404679d91270e63c3f947b3917a78d08446c9fa818aa0bf634037
arm64v1mingw=11361df6fd2404679d91270e63c3f947b3917a78d08446c9fa818aa0bf634037
arm64v1linux=a119c2f1aa2e8e442a16b74d1bec04090228a5d7ccde03dbc060b78cfd3b5c17
arm64v1musl=a119c2f1aa2e8e442a16b74d1bec04090228a5d7ccde03dbc060b78cfd3b5c17
arm64v1glibc=a119c2f1aa2e8e442a16b74d1bec04090228a5d7ccde03dbc060b78cfd3b5c17
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
