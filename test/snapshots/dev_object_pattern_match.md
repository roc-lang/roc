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
x64mac=3eaac7debf97d48422ce194171a85b3cd6dc60acfd2b149df82c228f285ea5a5
x64win=45b54793cd354a3d30294f5106210a8dda71e8cfba8f04ba9e17c9cf0f196c2a
x64mingw=45b54793cd354a3d30294f5106210a8dda71e8cfba8f04ba9e17c9cf0f196c2a
x64freebsd=e3b6c8bb6be0fc228231c08b984aae0e7a0955fb3377c94117f685e26e2302f0
x64openbsd=b6b1e553b73daea3ebdda5d5f5eb3310e821fc4d9fa1b5205375b4d8ed7cd011
x64netbsd=f99cb329f9a306201e94f4338443ccd30c5ac54e02a6dcabe961ab883e88af98
x64musl=f99cb329f9a306201e94f4338443ccd30c5ac54e02a6dcabe961ab883e88af98
x64glibc=f99cb329f9a306201e94f4338443ccd30c5ac54e02a6dcabe961ab883e88af98
x64linux=f99cb329f9a306201e94f4338443ccd30c5ac54e02a6dcabe961ab883e88af98
x64elf=f99cb329f9a306201e94f4338443ccd30c5ac54e02a6dcabe961ab883e88af98
x64v1mac=3eaac7debf97d48422ce194171a85b3cd6dc60acfd2b149df82c228f285ea5a5
x64v1win=45b54793cd354a3d30294f5106210a8dda71e8cfba8f04ba9e17c9cf0f196c2a
x64v1mingw=45b54793cd354a3d30294f5106210a8dda71e8cfba8f04ba9e17c9cf0f196c2a
x64v1freebsd=e3b6c8bb6be0fc228231c08b984aae0e7a0955fb3377c94117f685e26e2302f0
x64v1openbsd=b6b1e553b73daea3ebdda5d5f5eb3310e821fc4d9fa1b5205375b4d8ed7cd011
x64v1netbsd=f99cb329f9a306201e94f4338443ccd30c5ac54e02a6dcabe961ab883e88af98
x64v1musl=f99cb329f9a306201e94f4338443ccd30c5ac54e02a6dcabe961ab883e88af98
x64v1glibc=f99cb329f9a306201e94f4338443ccd30c5ac54e02a6dcabe961ab883e88af98
x64v1linux=f99cb329f9a306201e94f4338443ccd30c5ac54e02a6dcabe961ab883e88af98
x64v1elf=f99cb329f9a306201e94f4338443ccd30c5ac54e02a6dcabe961ab883e88af98
arm64mac=4b3bd27f8882741a3900f6156373301e97181d694489703b9cb5d7372e3f414d
arm64win=1fd7af3eb9b056ec3aec6cf78db80b9d73209ea40ae66b1f49c9ace9cd9cf01f
arm64mingw=1fd7af3eb9b056ec3aec6cf78db80b9d73209ea40ae66b1f49c9ace9cd9cf01f
arm64linux=b4294675fe009a1dc5e3c00796d63d96bbb3266c22e1ba81527c85b851254b32
arm64musl=b4294675fe009a1dc5e3c00796d63d96bbb3266c22e1ba81527c85b851254b32
arm64glibc=b4294675fe009a1dc5e3c00796d63d96bbb3266c22e1ba81527c85b851254b32
arm64v1win=1fd7af3eb9b056ec3aec6cf78db80b9d73209ea40ae66b1f49c9ace9cd9cf01f
arm64v1mingw=1fd7af3eb9b056ec3aec6cf78db80b9d73209ea40ae66b1f49c9ace9cd9cf01f
arm64v1linux=b4294675fe009a1dc5e3c00796d63d96bbb3266c22e1ba81527c85b851254b32
arm64v1musl=b4294675fe009a1dc5e3c00796d63d96bbb3266c22e1ba81527c85b851254b32
arm64v1glibc=b4294675fe009a1dc5e3c00796d63d96bbb3266c22e1ba81527c85b851254b32
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
