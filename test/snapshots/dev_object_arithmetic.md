# META
~~~ini
description=Integer arithmetic with I64 return type
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

main : I64
main = add(3, 4) * 2

add : I64, I64 -> I64
add = |a, b| a + b
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : I64 }
    exposes []
    packages {}
    provides { "roc_main": main_for_host }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
    }

main_for_host : I64
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# app
main = add(3, 4) * 2
add = |a, b| a + b

~~~
# DEV OUTPUT
~~~ini
x64mac=864eaa2bd1df8a727a013f028b4d54aa4a71fedf0332b9afa79179c25ebc408f
x64win=c6ef9e2e960d2707dd677dcc41a7c02b766c6a96476c6ce3900cbe737f0f61e0
x64mingw=c6ef9e2e960d2707dd677dcc41a7c02b766c6a96476c6ce3900cbe737f0f61e0
x64freebsd=f5fc2fa26b529f5088ee5cfd5e063ac6e4c06d88c9bdf4e28f9389afe48464ba
x64openbsd=0b2c753ec63753b32700ada898e4b3589781302d4fab1ff976f6f09016e38c86
x64netbsd=f27c55ccc5ea55391584f1851039256097e9fbc3ff8590ba9e36c1fa83a750ce
x64musl=f27c55ccc5ea55391584f1851039256097e9fbc3ff8590ba9e36c1fa83a750ce
x64glibc=f27c55ccc5ea55391584f1851039256097e9fbc3ff8590ba9e36c1fa83a750ce
x64linux=f27c55ccc5ea55391584f1851039256097e9fbc3ff8590ba9e36c1fa83a750ce
x64elf=f27c55ccc5ea55391584f1851039256097e9fbc3ff8590ba9e36c1fa83a750ce
x64v1mac=864eaa2bd1df8a727a013f028b4d54aa4a71fedf0332b9afa79179c25ebc408f
x64v1win=c6ef9e2e960d2707dd677dcc41a7c02b766c6a96476c6ce3900cbe737f0f61e0
x64v1mingw=c6ef9e2e960d2707dd677dcc41a7c02b766c6a96476c6ce3900cbe737f0f61e0
x64v1freebsd=f5fc2fa26b529f5088ee5cfd5e063ac6e4c06d88c9bdf4e28f9389afe48464ba
x64v1openbsd=0b2c753ec63753b32700ada898e4b3589781302d4fab1ff976f6f09016e38c86
x64v1netbsd=f27c55ccc5ea55391584f1851039256097e9fbc3ff8590ba9e36c1fa83a750ce
x64v1musl=f27c55ccc5ea55391584f1851039256097e9fbc3ff8590ba9e36c1fa83a750ce
x64v1glibc=f27c55ccc5ea55391584f1851039256097e9fbc3ff8590ba9e36c1fa83a750ce
x64v1linux=f27c55ccc5ea55391584f1851039256097e9fbc3ff8590ba9e36c1fa83a750ce
x64v1elf=f27c55ccc5ea55391584f1851039256097e9fbc3ff8590ba9e36c1fa83a750ce
arm64mac=fe088c22f4b2d05ec71b286be0f6625dce6d48c27216d4d49342734bb3a7137a
arm64win=79def94a1b7684218a82e70c29d515028847732ebc9f4d5ece5e0eeeb7bdbc75
arm64mingw=79def94a1b7684218a82e70c29d515028847732ebc9f4d5ece5e0eeeb7bdbc75
arm64linux=bd99f688eea049335099adb362c4f45b6515cef69988572672bb86afd3e386b7
arm64musl=bd99f688eea049335099adb362c4f45b6515cef69988572672bb86afd3e386b7
arm64glibc=bd99f688eea049335099adb362c4f45b6515cef69988572672bb86afd3e386b7
arm64v1win=79def94a1b7684218a82e70c29d515028847732ebc9f4d5ece5e0eeeb7bdbc75
arm64v1mingw=79def94a1b7684218a82e70c29d515028847732ebc9f4d5ece5e0eeeb7bdbc75
arm64v1linux=bd99f688eea049335099adb362c4f45b6515cef69988572672bb86afd3e386b7
arm64v1musl=bd99f688eea049335099adb362c4f45b6515cef69988572672bb86afd3e386b7
arm64v1glibc=bd99f688eea049335099adb362c4f45b6515cef69988572672bb86afd3e386b7
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
