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
    provides { main_for_host: "main" }
    targets: {
        files: "targets/",
        exe: {
            x64glibc: [app],
        }
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
x64mac=fa41456ec0e725c1490071d1e67304f805bffb185afcdca3b91268596abecf22
x64win=1daef17641e043f3ad6337cdf62da40514a74d4dbd0790db66fb5b2a8d322c32
x64freebsd=2c5b7aa4c2afd08a0788f217799d94d5838e30960c4b1c1667fd32a2044b4afc
x64openbsd=2c5b7aa4c2afd08a0788f217799d94d5838e30960c4b1c1667fd32a2044b4afc
x64netbsd=2c5b7aa4c2afd08a0788f217799d94d5838e30960c4b1c1667fd32a2044b4afc
x64musl=2c5b7aa4c2afd08a0788f217799d94d5838e30960c4b1c1667fd32a2044b4afc
x64glibc=2c5b7aa4c2afd08a0788f217799d94d5838e30960c4b1c1667fd32a2044b4afc
x64linux=2c5b7aa4c2afd08a0788f217799d94d5838e30960c4b1c1667fd32a2044b4afc
x64elf=2c5b7aa4c2afd08a0788f217799d94d5838e30960c4b1c1667fd32a2044b4afc
arm64mac=071b219aefd60457df7a8835aefb875e6b696eb1a2cda8041898edaa46f22f9a
arm64win=c25f1af5449d9203ccb6047a467adccde3caee7f094cfa7ddb0a7f7d201ba658
arm64linux=98c41aa3653222e2249e7af5916580ee5f042ee77eada9df38e11c7d786dc848
arm64musl=98c41aa3653222e2249e7af5916580ee5f042ee77eada9df38e11c7d786dc848
arm64glibc=98c41aa3653222e2249e7af5916580ee5f042ee77eada9df38e11c7d786dc848
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
