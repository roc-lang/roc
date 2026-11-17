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
x64mac=61bdf070ca14221968adc1dc901a7400f6cfc65ac13e3f0ad5d0efff7d412def
x64win=c778e609d704a254093bdbf5328d5602c236139f537ca01eef251fd6fdc46d5a
x64mingw=c778e609d704a254093bdbf5328d5602c236139f537ca01eef251fd6fdc46d5a
x64freebsd=25b35c494ee8965d348a1b8b7af8828a878b8a5b99677e0b866bb3f969312969
x64openbsd=9b168f5bcdbc1592c8dabbf5f3e0a24cf93481dddeae93244fb538aa0272704c
x64netbsd=58996bf25610c618370d54d32a8e5cf0c26c3b86d6bbdc652113a96ba85f0561
x64musl=58996bf25610c618370d54d32a8e5cf0c26c3b86d6bbdc652113a96ba85f0561
x64glibc=58996bf25610c618370d54d32a8e5cf0c26c3b86d6bbdc652113a96ba85f0561
x64linux=58996bf25610c618370d54d32a8e5cf0c26c3b86d6bbdc652113a96ba85f0561
x64elf=58996bf25610c618370d54d32a8e5cf0c26c3b86d6bbdc652113a96ba85f0561
x64v1mac=61bdf070ca14221968adc1dc901a7400f6cfc65ac13e3f0ad5d0efff7d412def
x64v1win=c778e609d704a254093bdbf5328d5602c236139f537ca01eef251fd6fdc46d5a
x64v1mingw=c778e609d704a254093bdbf5328d5602c236139f537ca01eef251fd6fdc46d5a
x64v1freebsd=25b35c494ee8965d348a1b8b7af8828a878b8a5b99677e0b866bb3f969312969
x64v1openbsd=9b168f5bcdbc1592c8dabbf5f3e0a24cf93481dddeae93244fb538aa0272704c
x64v1netbsd=58996bf25610c618370d54d32a8e5cf0c26c3b86d6bbdc652113a96ba85f0561
x64v1musl=58996bf25610c618370d54d32a8e5cf0c26c3b86d6bbdc652113a96ba85f0561
x64v1glibc=58996bf25610c618370d54d32a8e5cf0c26c3b86d6bbdc652113a96ba85f0561
x64v1linux=58996bf25610c618370d54d32a8e5cf0c26c3b86d6bbdc652113a96ba85f0561
x64v1elf=58996bf25610c618370d54d32a8e5cf0c26c3b86d6bbdc652113a96ba85f0561
arm64mac=36c14086fb6f3ee3cd57aa613bf12763a2b8cd976e1d2e70f7f79d93f96d802e
arm64win=ce95ca02df520aa113b7aa2ebf494d8321d9d7985e095f7f58fcdbe4a89fd8b2
arm64mingw=ce95ca02df520aa113b7aa2ebf494d8321d9d7985e095f7f58fcdbe4a89fd8b2
arm64linux=ca168e6f7aaf8e10dbb7bfef98ba06212b43feb264e70beecca4541ce77faecd
arm64musl=ca168e6f7aaf8e10dbb7bfef98ba06212b43feb264e70beecca4541ce77faecd
arm64glibc=ca168e6f7aaf8e10dbb7bfef98ba06212b43feb264e70beecca4541ce77faecd
arm64v1win=ce95ca02df520aa113b7aa2ebf494d8321d9d7985e095f7f58fcdbe4a89fd8b2
arm64v1mingw=ce95ca02df520aa113b7aa2ebf494d8321d9d7985e095f7f58fcdbe4a89fd8b2
arm64v1linux=ca168e6f7aaf8e10dbb7bfef98ba06212b43feb264e70beecca4541ce77faecd
arm64v1musl=ca168e6f7aaf8e10dbb7bfef98ba06212b43feb264e70beecca4541ce77faecd
arm64v1glibc=ca168e6f7aaf8e10dbb7bfef98ba06212b43feb264e70beecca4541ce77faecd
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
