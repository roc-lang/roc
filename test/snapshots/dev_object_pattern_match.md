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
x64mac=dda118b88b65428eac251182f2a1d0535d2f280247ba9f688768388051aa260d
x64win=c93179f77da5885dfe720579d5d068adf24cde25b4a93eb5514d5812786b4630
x64mingw=c93179f77da5885dfe720579d5d068adf24cde25b4a93eb5514d5812786b4630
x64freebsd=47d161cbdaac7d78bcbefda6734c15e3749f62d8031dabbd6d834a4968f2e2d6
x64openbsd=e12cae7879b30f2497ec95c904d1525444db53ae6492dc68091fa7b868d528ff
x64netbsd=c3959674d9413cafd975e0f1dd143e02dd1f631ddb089045919074c5c58b6ee6
x64musl=c3959674d9413cafd975e0f1dd143e02dd1f631ddb089045919074c5c58b6ee6
x64glibc=c3959674d9413cafd975e0f1dd143e02dd1f631ddb089045919074c5c58b6ee6
x64linux=c3959674d9413cafd975e0f1dd143e02dd1f631ddb089045919074c5c58b6ee6
x64elf=c3959674d9413cafd975e0f1dd143e02dd1f631ddb089045919074c5c58b6ee6
x64v1mac=dda118b88b65428eac251182f2a1d0535d2f280247ba9f688768388051aa260d
x64v1win=c93179f77da5885dfe720579d5d068adf24cde25b4a93eb5514d5812786b4630
x64v1mingw=c93179f77da5885dfe720579d5d068adf24cde25b4a93eb5514d5812786b4630
x64v1freebsd=47d161cbdaac7d78bcbefda6734c15e3749f62d8031dabbd6d834a4968f2e2d6
x64v1openbsd=e12cae7879b30f2497ec95c904d1525444db53ae6492dc68091fa7b868d528ff
x64v1netbsd=c3959674d9413cafd975e0f1dd143e02dd1f631ddb089045919074c5c58b6ee6
x64v1musl=c3959674d9413cafd975e0f1dd143e02dd1f631ddb089045919074c5c58b6ee6
x64v1glibc=c3959674d9413cafd975e0f1dd143e02dd1f631ddb089045919074c5c58b6ee6
x64v1linux=c3959674d9413cafd975e0f1dd143e02dd1f631ddb089045919074c5c58b6ee6
x64v1elf=c3959674d9413cafd975e0f1dd143e02dd1f631ddb089045919074c5c58b6ee6
arm64mac=94cb73f5e2145fbbd24b7e2fe5d7a40cf1e7f9b44e2714c655d3d1ac5c1b17db
arm64win=602366ff10ffa7bd3d28905e25cbb67a37e8e9fb7b3a4f8ea971b8268207fef5
arm64mingw=602366ff10ffa7bd3d28905e25cbb67a37e8e9fb7b3a4f8ea971b8268207fef5
arm64linux=066361de9027453033b6ee8f6a1180f36324e642ed54813a807c11bcc7e591fc
arm64musl=066361de9027453033b6ee8f6a1180f36324e642ed54813a807c11bcc7e591fc
arm64glibc=066361de9027453033b6ee8f6a1180f36324e642ed54813a807c11bcc7e591fc
arm64v1win=602366ff10ffa7bd3d28905e25cbb67a37e8e9fb7b3a4f8ea971b8268207fef5
arm64v1mingw=602366ff10ffa7bd3d28905e25cbb67a37e8e9fb7b3a4f8ea971b8268207fef5
arm64v1linux=066361de9027453033b6ee8f6a1180f36324e642ed54813a807c11bcc7e591fc
arm64v1musl=066361de9027453033b6ee8f6a1180f36324e642ed54813a807c11bcc7e591fc
arm64v1glibc=066361de9027453033b6ee8f6a1180f36324e642ed54813a807c11bcc7e591fc
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
