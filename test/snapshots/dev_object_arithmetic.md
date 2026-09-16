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
x64mac=d34824fb540de587c19f72eab03a8a1bdd7c758a2d9fa362f5f809b85f423741
x64win=c13d631a75eff1b639f1e5640b90d3d43a6b739b4c4e191e2c6a2dfc78714063
x64mingw=c13d631a75eff1b639f1e5640b90d3d43a6b739b4c4e191e2c6a2dfc78714063
x64freebsd=70124784c49e77247c228f5f78857f10ecdc994ad37e965c59925d243865f4c8
x64openbsd=6d52fc399d2ffa6fbce5c72ce728ddc1e81090871b008cb7de620d3d6fc90fe5
x64netbsd=f57a343256a8b6e738c0a42c50169d6cce0e731b72950971a04c06144a6f612f
x64musl=f57a343256a8b6e738c0a42c50169d6cce0e731b72950971a04c06144a6f612f
x64glibc=f57a343256a8b6e738c0a42c50169d6cce0e731b72950971a04c06144a6f612f
x64linux=f57a343256a8b6e738c0a42c50169d6cce0e731b72950971a04c06144a6f612f
x64elf=f57a343256a8b6e738c0a42c50169d6cce0e731b72950971a04c06144a6f612f
x64v1mac=d34824fb540de587c19f72eab03a8a1bdd7c758a2d9fa362f5f809b85f423741
x64v1win=c13d631a75eff1b639f1e5640b90d3d43a6b739b4c4e191e2c6a2dfc78714063
x64v1mingw=c13d631a75eff1b639f1e5640b90d3d43a6b739b4c4e191e2c6a2dfc78714063
x64v1freebsd=70124784c49e77247c228f5f78857f10ecdc994ad37e965c59925d243865f4c8
x64v1openbsd=6d52fc399d2ffa6fbce5c72ce728ddc1e81090871b008cb7de620d3d6fc90fe5
x64v1netbsd=f57a343256a8b6e738c0a42c50169d6cce0e731b72950971a04c06144a6f612f
x64v1musl=f57a343256a8b6e738c0a42c50169d6cce0e731b72950971a04c06144a6f612f
x64v1glibc=f57a343256a8b6e738c0a42c50169d6cce0e731b72950971a04c06144a6f612f
x64v1linux=f57a343256a8b6e738c0a42c50169d6cce0e731b72950971a04c06144a6f612f
x64v1elf=f57a343256a8b6e738c0a42c50169d6cce0e731b72950971a04c06144a6f612f
arm64mac=1ceee0a039b5142946c0daba6cc71e455abeb02868548cc050d5b90bab647a47
arm64win=dbf327b2998b647b6731ba24bc7ee6fa726e95eea61e7de43faeb1e06c356d39
arm64mingw=dbf327b2998b647b6731ba24bc7ee6fa726e95eea61e7de43faeb1e06c356d39
arm64linux=d430d5ed6918bb8dfbfa816efce5803f8f0c19bc68f20e6b8be27802234f2c55
arm64musl=d430d5ed6918bb8dfbfa816efce5803f8f0c19bc68f20e6b8be27802234f2c55
arm64glibc=d430d5ed6918bb8dfbfa816efce5803f8f0c19bc68f20e6b8be27802234f2c55
arm64v1win=dbf327b2998b647b6731ba24bc7ee6fa726e95eea61e7de43faeb1e06c356d39
arm64v1mingw=dbf327b2998b647b6731ba24bc7ee6fa726e95eea61e7de43faeb1e06c356d39
arm64v1linux=d430d5ed6918bb8dfbfa816efce5803f8f0c19bc68f20e6b8be27802234f2c55
arm64v1musl=d430d5ed6918bb8dfbfa816efce5803f8f0c19bc68f20e6b8be27802234f2c55
arm64v1glibc=d430d5ed6918bb8dfbfa816efce5803f8f0c19bc68f20e6b8be27802234f2c55
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
