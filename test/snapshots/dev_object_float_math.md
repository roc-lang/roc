# META
~~~ini
description=F32/F64 arithmetic, comparisons, and F64 to I64 conversion in host-called functions
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

main : F64, F32, I64 -> I64
main = |x, y, n| {
    scaled = x * 2.5 - I64.to_f64(n) / 3.0
    narrowed = F32.to_f64(y * 1.5 + 0.25)
    total = if scaled > narrowed { scaled - narrowed } else { narrowed + scaled }
    F64.to_i64_wrap(total)
}
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : F64, F32, I64 -> I64 }
    exposes []
    packages {}
    provides { "roc_main": main_for_host }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
    }

main_for_host : F64, F32, I64 -> I64
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# app
main = |x, y, n| {
	scaled = x * 2.5 - to_f64(n) / 3
	narrowed = to_f64(y * 1.5 + 0.25)
	total = if (scaled > narrowed) {
		scaled - narrowed
	} else {
		narrowed + scaled
	}
	to_i64_wrap(total)
}

~~~
# DEV OUTPUT
~~~ini
x64mac=236bd2fbceaf5ef50e6b25cc7c85858b0aa813ee83e9808323b914f6b5b2e615
x64win=55505636d589e258246e786e62364feda2182588f89e2e39c9eb96204800a4a7
x64mingw=55505636d589e258246e786e62364feda2182588f89e2e39c9eb96204800a4a7
x64freebsd=51e8ec7ab2f8dc854a8199416c42380539b98a58213bbd00c4ea58c88e9ce613
x64openbsd=e2db2c70cb68e9368cf3d509e3ab03085e093c36e044478deb9ed4f679071198
x64netbsd=ef09908082f653136803659e680c553892f5fc7551d9041c312b666e16809c2a
x64musl=ef09908082f653136803659e680c553892f5fc7551d9041c312b666e16809c2a
x64glibc=ef09908082f653136803659e680c553892f5fc7551d9041c312b666e16809c2a
x64linux=ef09908082f653136803659e680c553892f5fc7551d9041c312b666e16809c2a
x64elf=ef09908082f653136803659e680c553892f5fc7551d9041c312b666e16809c2a
x64v1mac=236bd2fbceaf5ef50e6b25cc7c85858b0aa813ee83e9808323b914f6b5b2e615
x64v1win=55505636d589e258246e786e62364feda2182588f89e2e39c9eb96204800a4a7
x64v1mingw=55505636d589e258246e786e62364feda2182588f89e2e39c9eb96204800a4a7
x64v1freebsd=51e8ec7ab2f8dc854a8199416c42380539b98a58213bbd00c4ea58c88e9ce613
x64v1openbsd=e2db2c70cb68e9368cf3d509e3ab03085e093c36e044478deb9ed4f679071198
x64v1netbsd=ef09908082f653136803659e680c553892f5fc7551d9041c312b666e16809c2a
x64v1musl=ef09908082f653136803659e680c553892f5fc7551d9041c312b666e16809c2a
x64v1glibc=ef09908082f653136803659e680c553892f5fc7551d9041c312b666e16809c2a
x64v1linux=ef09908082f653136803659e680c553892f5fc7551d9041c312b666e16809c2a
x64v1elf=ef09908082f653136803659e680c553892f5fc7551d9041c312b666e16809c2a
arm64mac=b48106753d425b435e0be1a33de021c71a2828f9b6d4a43d554eb8597c9b6e41
arm64win=99ad5ca6863c5609308226e2a06cbd2eb5bd6fdcb71b0c7e72670f9496f9340d
arm64mingw=99ad5ca6863c5609308226e2a06cbd2eb5bd6fdcb71b0c7e72670f9496f9340d
arm64linux=1ad0614fcfb87f2185046a30a2f2f981bc5cce001c03a20c91f316f3e07a8a59
arm64musl=1ad0614fcfb87f2185046a30a2f2f981bc5cce001c03a20c91f316f3e07a8a59
arm64glibc=1ad0614fcfb87f2185046a30a2f2f981bc5cce001c03a20c91f316f3e07a8a59
arm64v1win=99ad5ca6863c5609308226e2a06cbd2eb5bd6fdcb71b0c7e72670f9496f9340d
arm64v1mingw=99ad5ca6863c5609308226e2a06cbd2eb5bd6fdcb71b0c7e72670f9496f9340d
arm64v1linux=1ad0614fcfb87f2185046a30a2f2f981bc5cce001c03a20c91f316f3e07a8a59
arm64v1musl=1ad0614fcfb87f2185046a30a2f2f981bc5cce001c03a20c91f316f3e07a8a59
arm64v1glibc=1ad0614fcfb87f2185046a30a2f2f981bc5cce001c03a20c91f316f3e07a8a59
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
