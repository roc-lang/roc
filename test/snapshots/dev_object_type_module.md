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
x64mac=7482bb27c3e9937f593f5d7e8ccb1de820d6255d7d079b84385c941950c0647e
x64win=d98aeb1e11dfe04de3ec643532d8d60df70ef704d0ab880b242af6813ea4fe3f
x64mingw=d98aeb1e11dfe04de3ec643532d8d60df70ef704d0ab880b242af6813ea4fe3f
x64freebsd=2df1e0360295631554bce59683846d53523ae5b44e6d2588769c857ede1294d1
x64openbsd=cdbadf1b031a6844bf7c803c9ae1724f8a738483e798d031fd0c2e0e9dc68186
x64netbsd=1964a4a86c6839fce4c231210fa1ab62b7cfad480dde3abb5e640471c40a9724
x64musl=1964a4a86c6839fce4c231210fa1ab62b7cfad480dde3abb5e640471c40a9724
x64glibc=1964a4a86c6839fce4c231210fa1ab62b7cfad480dde3abb5e640471c40a9724
x64linux=1964a4a86c6839fce4c231210fa1ab62b7cfad480dde3abb5e640471c40a9724
x64elf=1964a4a86c6839fce4c231210fa1ab62b7cfad480dde3abb5e640471c40a9724
x64v1mac=7482bb27c3e9937f593f5d7e8ccb1de820d6255d7d079b84385c941950c0647e
x64v1win=d98aeb1e11dfe04de3ec643532d8d60df70ef704d0ab880b242af6813ea4fe3f
x64v1mingw=d98aeb1e11dfe04de3ec643532d8d60df70ef704d0ab880b242af6813ea4fe3f
x64v1freebsd=2df1e0360295631554bce59683846d53523ae5b44e6d2588769c857ede1294d1
x64v1openbsd=cdbadf1b031a6844bf7c803c9ae1724f8a738483e798d031fd0c2e0e9dc68186
x64v1netbsd=1964a4a86c6839fce4c231210fa1ab62b7cfad480dde3abb5e640471c40a9724
x64v1musl=1964a4a86c6839fce4c231210fa1ab62b7cfad480dde3abb5e640471c40a9724
x64v1glibc=1964a4a86c6839fce4c231210fa1ab62b7cfad480dde3abb5e640471c40a9724
x64v1linux=1964a4a86c6839fce4c231210fa1ab62b7cfad480dde3abb5e640471c40a9724
x64v1elf=1964a4a86c6839fce4c231210fa1ab62b7cfad480dde3abb5e640471c40a9724
arm64mac=8d75c2f14efbd6dcedaf08c25a0fa43956d6d0b37a16b2ac95e17358c0e28b7d
arm64win=d1fbd67abdd260c3e3759fd407d58e6bab53a4cf2f6535483decbb6f41cf808a
arm64mingw=d1fbd67abdd260c3e3759fd407d58e6bab53a4cf2f6535483decbb6f41cf808a
arm64linux=d539909179506cc1a9666e7fa1d8f52de754aafa49d193eced251d5560d4bc6e
arm64musl=d539909179506cc1a9666e7fa1d8f52de754aafa49d193eced251d5560d4bc6e
arm64glibc=d539909179506cc1a9666e7fa1d8f52de754aafa49d193eced251d5560d4bc6e
arm64v1win=d1fbd67abdd260c3e3759fd407d58e6bab53a4cf2f6535483decbb6f41cf808a
arm64v1mingw=d1fbd67abdd260c3e3759fd407d58e6bab53a4cf2f6535483decbb6f41cf808a
arm64v1linux=d539909179506cc1a9666e7fa1d8f52de754aafa49d193eced251d5560d4bc6e
arm64v1musl=d539909179506cc1a9666e7fa1d8f52de754aafa49d193eced251d5560d4bc6e
arm64v1glibc=d539909179506cc1a9666e7fa1d8f52de754aafa49d193eced251d5560d4bc6e
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
