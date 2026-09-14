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
x64mac=d7a771a4d6df844b0f5c39a8e6579b2169659d62ade9ee9e339ed9b49cc9cc06
x64win=c65303b8fb0d5c579abcaf6c7b5793b27ac9ad0fa6fbaf8bf8a407070c2fc79a
x64mingw=c65303b8fb0d5c579abcaf6c7b5793b27ac9ad0fa6fbaf8bf8a407070c2fc79a
x64freebsd=162a7ea3c3dd0e539652972166b04219c45b9530e3e8a0ac968d2bca65300d79
x64openbsd=030dcf0053cd6e5aa7ac4236afeb2079faf21f097ef8d13b83cb46af17f06cc3
x64netbsd=4a8d4459318535f0289e40f2e5015de000c783b90145c115fd29023cddfde888
x64musl=4a8d4459318535f0289e40f2e5015de000c783b90145c115fd29023cddfde888
x64glibc=4a8d4459318535f0289e40f2e5015de000c783b90145c115fd29023cddfde888
x64linux=4a8d4459318535f0289e40f2e5015de000c783b90145c115fd29023cddfde888
x64elf=4a8d4459318535f0289e40f2e5015de000c783b90145c115fd29023cddfde888
x64v1mac=d7a771a4d6df844b0f5c39a8e6579b2169659d62ade9ee9e339ed9b49cc9cc06
x64v1win=c65303b8fb0d5c579abcaf6c7b5793b27ac9ad0fa6fbaf8bf8a407070c2fc79a
x64v1mingw=c65303b8fb0d5c579abcaf6c7b5793b27ac9ad0fa6fbaf8bf8a407070c2fc79a
x64v1freebsd=162a7ea3c3dd0e539652972166b04219c45b9530e3e8a0ac968d2bca65300d79
x64v1openbsd=030dcf0053cd6e5aa7ac4236afeb2079faf21f097ef8d13b83cb46af17f06cc3
x64v1netbsd=4a8d4459318535f0289e40f2e5015de000c783b90145c115fd29023cddfde888
x64v1musl=4a8d4459318535f0289e40f2e5015de000c783b90145c115fd29023cddfde888
x64v1glibc=4a8d4459318535f0289e40f2e5015de000c783b90145c115fd29023cddfde888
x64v1linux=4a8d4459318535f0289e40f2e5015de000c783b90145c115fd29023cddfde888
x64v1elf=4a8d4459318535f0289e40f2e5015de000c783b90145c115fd29023cddfde888
arm64mac=c90a2072e18bc9813ba813b133d88e8276ddff8947f96787f5ed49daac4f3865
arm64win=5a9c9ec644c4c9cb5228ff473a59daccfbcf9facfbcf897ae671fbd18cca663d
arm64mingw=5a9c9ec644c4c9cb5228ff473a59daccfbcf9facfbcf897ae671fbd18cca663d
arm64linux=ff96d8ef157a773728f06f1ccf694a994cc1cf31c63c02ab3bad2196493fddc4
arm64musl=ff96d8ef157a773728f06f1ccf694a994cc1cf31c63c02ab3bad2196493fddc4
arm64glibc=ff96d8ef157a773728f06f1ccf694a994cc1cf31c63c02ab3bad2196493fddc4
arm64v1win=5a9c9ec644c4c9cb5228ff473a59daccfbcf9facfbcf897ae671fbd18cca663d
arm64v1mingw=5a9c9ec644c4c9cb5228ff473a59daccfbcf9facfbcf897ae671fbd18cca663d
arm64v1linux=ff96d8ef157a773728f06f1ccf694a994cc1cf31c63c02ab3bad2196493fddc4
arm64v1musl=ff96d8ef157a773728f06f1ccf694a994cc1cf31c63c02ab3bad2196493fddc4
arm64v1glibc=ff96d8ef157a773728f06f1ccf694a994cc1cf31c63c02ab3bad2196493fddc4
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
