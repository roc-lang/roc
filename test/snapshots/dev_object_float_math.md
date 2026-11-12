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
x64mac=c9dee0219027c80af4d437960c74386de4fd23f99af6785c6183b9f980f41c56
x64win=d7229b60593a6a9662287e5d699fc4763e91e52631cf9ee0c4b1998c3eed55a0
x64mingw=d7229b60593a6a9662287e5d699fc4763e91e52631cf9ee0c4b1998c3eed55a0
x64freebsd=0cfe44c42946ddd0801615a9b3b13376cdcecffb8f2faaa3b9b2062f6ec2ddff
x64openbsd=a83d1eba3c7cdd95e06353f377f39f5c9fdd44449d47dcb35e1a77aaa8c23118
x64netbsd=93d393fd228ce9ded790acaea97884a4bdce9ce64e45bc319bf964939465e4c1
x64musl=93d393fd228ce9ded790acaea97884a4bdce9ce64e45bc319bf964939465e4c1
x64glibc=93d393fd228ce9ded790acaea97884a4bdce9ce64e45bc319bf964939465e4c1
x64linux=93d393fd228ce9ded790acaea97884a4bdce9ce64e45bc319bf964939465e4c1
x64elf=93d393fd228ce9ded790acaea97884a4bdce9ce64e45bc319bf964939465e4c1
x64v1mac=c9dee0219027c80af4d437960c74386de4fd23f99af6785c6183b9f980f41c56
x64v1win=d7229b60593a6a9662287e5d699fc4763e91e52631cf9ee0c4b1998c3eed55a0
x64v1mingw=d7229b60593a6a9662287e5d699fc4763e91e52631cf9ee0c4b1998c3eed55a0
x64v1freebsd=0cfe44c42946ddd0801615a9b3b13376cdcecffb8f2faaa3b9b2062f6ec2ddff
x64v1openbsd=a83d1eba3c7cdd95e06353f377f39f5c9fdd44449d47dcb35e1a77aaa8c23118
x64v1netbsd=93d393fd228ce9ded790acaea97884a4bdce9ce64e45bc319bf964939465e4c1
x64v1musl=93d393fd228ce9ded790acaea97884a4bdce9ce64e45bc319bf964939465e4c1
x64v1glibc=93d393fd228ce9ded790acaea97884a4bdce9ce64e45bc319bf964939465e4c1
x64v1linux=93d393fd228ce9ded790acaea97884a4bdce9ce64e45bc319bf964939465e4c1
x64v1elf=93d393fd228ce9ded790acaea97884a4bdce9ce64e45bc319bf964939465e4c1
arm64mac=d938e8d0bd76f92cfe9539506f13755b340c0a996a693cb40a286d47b687c7e2
arm64win=cba5157049befb85f3a69870d34b9da07de469b4c77070b6d6046d9f703160b8
arm64mingw=cba5157049befb85f3a69870d34b9da07de469b4c77070b6d6046d9f703160b8
arm64linux=b53257d95b047a06f2e75e08e614d49cdb45bb68e1f8f4762187c3ddbc8c4672
arm64musl=b53257d95b047a06f2e75e08e614d49cdb45bb68e1f8f4762187c3ddbc8c4672
arm64glibc=b53257d95b047a06f2e75e08e614d49cdb45bb68e1f8f4762187c3ddbc8c4672
arm64v1win=cba5157049befb85f3a69870d34b9da07de469b4c77070b6d6046d9f703160b8
arm64v1mingw=cba5157049befb85f3a69870d34b9da07de469b4c77070b6d6046d9f703160b8
arm64v1linux=b53257d95b047a06f2e75e08e614d49cdb45bb68e1f8f4762187c3ddbc8c4672
arm64v1musl=b53257d95b047a06f2e75e08e614d49cdb45bb68e1f8f4762187c3ddbc8c4672
arm64v1glibc=b53257d95b047a06f2e75e08e614d49cdb45bb68e1f8f4762187c3ddbc8c4672
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
