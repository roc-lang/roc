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
x64mac=8f5a6688ba57dcc7889369bda331e074de25efcbf8fe74c49c2a1ce19d7722c5
x64win=5e86e645498604a1c1fb7ccf2db8ae76e4ac50b511286ab06bdc8eaa244243a1
x64mingw=5e86e645498604a1c1fb7ccf2db8ae76e4ac50b511286ab06bdc8eaa244243a1
x64freebsd=d39181c3fb4879e6f8bfadd16277e4a453a180b51d0bae4d4b619015c1f15677
x64openbsd=608fb67260cf5cc982045f326f492593f884995c3d8d19ca81c2b4e8faffa3e1
x64netbsd=bbd476ee3e0e021f1ee4419328ac2edb6482268ee47e56a53257c6baf53a6cf6
x64musl=bbd476ee3e0e021f1ee4419328ac2edb6482268ee47e56a53257c6baf53a6cf6
x64glibc=bbd476ee3e0e021f1ee4419328ac2edb6482268ee47e56a53257c6baf53a6cf6
x64linux=bbd476ee3e0e021f1ee4419328ac2edb6482268ee47e56a53257c6baf53a6cf6
x64elf=bbd476ee3e0e021f1ee4419328ac2edb6482268ee47e56a53257c6baf53a6cf6
x64v1mac=8f5a6688ba57dcc7889369bda331e074de25efcbf8fe74c49c2a1ce19d7722c5
x64v1win=5e86e645498604a1c1fb7ccf2db8ae76e4ac50b511286ab06bdc8eaa244243a1
x64v1mingw=5e86e645498604a1c1fb7ccf2db8ae76e4ac50b511286ab06bdc8eaa244243a1
x64v1freebsd=d39181c3fb4879e6f8bfadd16277e4a453a180b51d0bae4d4b619015c1f15677
x64v1openbsd=608fb67260cf5cc982045f326f492593f884995c3d8d19ca81c2b4e8faffa3e1
x64v1netbsd=bbd476ee3e0e021f1ee4419328ac2edb6482268ee47e56a53257c6baf53a6cf6
x64v1musl=bbd476ee3e0e021f1ee4419328ac2edb6482268ee47e56a53257c6baf53a6cf6
x64v1glibc=bbd476ee3e0e021f1ee4419328ac2edb6482268ee47e56a53257c6baf53a6cf6
x64v1linux=bbd476ee3e0e021f1ee4419328ac2edb6482268ee47e56a53257c6baf53a6cf6
x64v1elf=bbd476ee3e0e021f1ee4419328ac2edb6482268ee47e56a53257c6baf53a6cf6
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
