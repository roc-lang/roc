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
x64mac=b8c47d9936b54a7524f463dc22f51fb8fccae54fa6c5e3c9ad4b137b9db12506
x64win=8e80f86f44fc189dfdccc92ad8411134952f423c8432e46a48e981c61d6f4a36
x64mingw=8e80f86f44fc189dfdccc92ad8411134952f423c8432e46a48e981c61d6f4a36
x64freebsd=3c91171e021b94c6f65346a3608bfc7adf631a61b71ec0ca9f7860e79c726824
x64openbsd=12e5da6734c94db630d6d52423fd52c44f44fd1b73c0befe72cb862477724c80
x64netbsd=f3d26edb4f74efddf9e56f435093eaa5c8f02d85911564cdcb2615b86ebdd2e0
x64musl=f3d26edb4f74efddf9e56f435093eaa5c8f02d85911564cdcb2615b86ebdd2e0
x64glibc=f3d26edb4f74efddf9e56f435093eaa5c8f02d85911564cdcb2615b86ebdd2e0
x64linux=f3d26edb4f74efddf9e56f435093eaa5c8f02d85911564cdcb2615b86ebdd2e0
x64elf=f3d26edb4f74efddf9e56f435093eaa5c8f02d85911564cdcb2615b86ebdd2e0
x64v1mac=b8c47d9936b54a7524f463dc22f51fb8fccae54fa6c5e3c9ad4b137b9db12506
x64v1win=8e80f86f44fc189dfdccc92ad8411134952f423c8432e46a48e981c61d6f4a36
x64v1mingw=8e80f86f44fc189dfdccc92ad8411134952f423c8432e46a48e981c61d6f4a36
x64v1freebsd=3c91171e021b94c6f65346a3608bfc7adf631a61b71ec0ca9f7860e79c726824
x64v1openbsd=12e5da6734c94db630d6d52423fd52c44f44fd1b73c0befe72cb862477724c80
x64v1netbsd=f3d26edb4f74efddf9e56f435093eaa5c8f02d85911564cdcb2615b86ebdd2e0
x64v1musl=f3d26edb4f74efddf9e56f435093eaa5c8f02d85911564cdcb2615b86ebdd2e0
x64v1glibc=f3d26edb4f74efddf9e56f435093eaa5c8f02d85911564cdcb2615b86ebdd2e0
x64v1linux=f3d26edb4f74efddf9e56f435093eaa5c8f02d85911564cdcb2615b86ebdd2e0
x64v1elf=f3d26edb4f74efddf9e56f435093eaa5c8f02d85911564cdcb2615b86ebdd2e0
arm64mac=63d7c7f23458b409cf1813afabc4889d2c3e8baa3bd682e6b45ea1ef6169bfaa
arm64win=8c30378f04ef944d3304b1915c7e4e1a80286fbddbe95722b4eec74a177d1fcf
arm64mingw=8c30378f04ef944d3304b1915c7e4e1a80286fbddbe95722b4eec74a177d1fcf
arm64linux=ba4276e8b66fe840e18a46511a85ab0eabb036ebcfed1a418906eeae80d9476c
arm64musl=ba4276e8b66fe840e18a46511a85ab0eabb036ebcfed1a418906eeae80d9476c
arm64glibc=ba4276e8b66fe840e18a46511a85ab0eabb036ebcfed1a418906eeae80d9476c
arm64v1win=8c30378f04ef944d3304b1915c7e4e1a80286fbddbe95722b4eec74a177d1fcf
arm64v1mingw=8c30378f04ef944d3304b1915c7e4e1a80286fbddbe95722b4eec74a177d1fcf
arm64v1linux=ba4276e8b66fe840e18a46511a85ab0eabb036ebcfed1a418906eeae80d9476c
arm64v1musl=ba4276e8b66fe840e18a46511a85ab0eabb036ebcfed1a418906eeae80d9476c
arm64v1glibc=ba4276e8b66fe840e18a46511a85ab0eabb036ebcfed1a418906eeae80d9476c
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
