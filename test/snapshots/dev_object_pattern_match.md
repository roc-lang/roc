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
        inputs: "targets/",
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
x64mac=8ff2f1fae197b222d44f9a20443eae93826a3b185cceecabe7ab0ecb418e6bff
x64win=828583c59228a6062a44f2feb6d9e042fbfb728e04b548aba5f23c0866092bcb
x64freebsd=b0d2921e6e85e40f594c1d8ab49216ba6dc86580d29679d227ea735a9d9868b9
x64openbsd=b0d2921e6e85e40f594c1d8ab49216ba6dc86580d29679d227ea735a9d9868b9
x64netbsd=b0d2921e6e85e40f594c1d8ab49216ba6dc86580d29679d227ea735a9d9868b9
x64musl=b0d2921e6e85e40f594c1d8ab49216ba6dc86580d29679d227ea735a9d9868b9
x64glibc=b0d2921e6e85e40f594c1d8ab49216ba6dc86580d29679d227ea735a9d9868b9
x64linux=b0d2921e6e85e40f594c1d8ab49216ba6dc86580d29679d227ea735a9d9868b9
x64elf=b0d2921e6e85e40f594c1d8ab49216ba6dc86580d29679d227ea735a9d9868b9
arm64mac=59c68039a11afdb37e8c20435d3e8a2b285573feb4e9997e54bc9caa70799fd1
arm64win=008c53d141d34720afdd3eb7edf190fd05b021584945bde81c957235de1faf13
arm64linux=65ffac65c4d5688025e1e81c8d98b72e97706ee99ef023a8cd5dd75595b31884
arm64musl=65ffac65c4d5688025e1e81c8d98b72e97706ee99ef023a8cd5dd75595b31884
arm64glibc=65ffac65c4d5688025e1e81c8d98b72e97706ee99ef023a8cd5dd75595b31884
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
~~~
