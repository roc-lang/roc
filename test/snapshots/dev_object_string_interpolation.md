# META
~~~ini
description=String interpolation and concatenation
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

greeting = "Hello"
name = "World"
main = "${greeting}, ${name}!"
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
greeting = "Hello"
name = "World"
main = {
	cinterp_0 = greeting
	cinterp_1 = name
	<interpolation>("", [cinterp_0, ", ", cinterp_1, "!"])
}

~~~
# DEV OUTPUT
~~~ini
x64mac=d13788626f499f422bfe17bd08405eaee296a7669c851262c4787478239981d6
x64win=1c8a197ee83cd31d99c78139d142e113ebb1b7ed9ecdf9a7cd58b5513a39bf9f
x64mingw=1c8a197ee83cd31d99c78139d142e113ebb1b7ed9ecdf9a7cd58b5513a39bf9f
x64freebsd=2b378053aa36fa2bd0ce66641300968db96a57eba85a664f96991af825e88a44
x64openbsd=262a81b9ae7190a57318614eda15d4caedb4a57b98f87a1c071973462d2756ac
x64netbsd=373ceca2a393d81cd38fd299088f76444d26ddf25174436e5d220f6c1f632ce6
x64musl=373ceca2a393d81cd38fd299088f76444d26ddf25174436e5d220f6c1f632ce6
x64glibc=373ceca2a393d81cd38fd299088f76444d26ddf25174436e5d220f6c1f632ce6
x64linux=373ceca2a393d81cd38fd299088f76444d26ddf25174436e5d220f6c1f632ce6
x64elf=373ceca2a393d81cd38fd299088f76444d26ddf25174436e5d220f6c1f632ce6
x64v1mac=d13788626f499f422bfe17bd08405eaee296a7669c851262c4787478239981d6
x64v1win=1c8a197ee83cd31d99c78139d142e113ebb1b7ed9ecdf9a7cd58b5513a39bf9f
x64v1mingw=1c8a197ee83cd31d99c78139d142e113ebb1b7ed9ecdf9a7cd58b5513a39bf9f
x64v1freebsd=2b378053aa36fa2bd0ce66641300968db96a57eba85a664f96991af825e88a44
x64v1openbsd=262a81b9ae7190a57318614eda15d4caedb4a57b98f87a1c071973462d2756ac
x64v1netbsd=373ceca2a393d81cd38fd299088f76444d26ddf25174436e5d220f6c1f632ce6
x64v1musl=373ceca2a393d81cd38fd299088f76444d26ddf25174436e5d220f6c1f632ce6
x64v1glibc=373ceca2a393d81cd38fd299088f76444d26ddf25174436e5d220f6c1f632ce6
x64v1linux=373ceca2a393d81cd38fd299088f76444d26ddf25174436e5d220f6c1f632ce6
x64v1elf=373ceca2a393d81cd38fd299088f76444d26ddf25174436e5d220f6c1f632ce6
arm64mac=97f3a867bd8b597252778e616bae356597caa8f2729bf79a84abd5b4fcfff432
arm64win=b05a3f4a0a72339e0af917399600eba6dd4e6cdacac2aefbb48b6f37abc34737
arm64mingw=b05a3f4a0a72339e0af917399600eba6dd4e6cdacac2aefbb48b6f37abc34737
arm64linux=82d468a664204ef632e1d6fb85e05069a628f10c49afd63e258aad26e1ff82a2
arm64musl=82d468a664204ef632e1d6fb85e05069a628f10c49afd63e258aad26e1ff82a2
arm64glibc=82d468a664204ef632e1d6fb85e05069a628f10c49afd63e258aad26e1ff82a2
arm64v1win=b05a3f4a0a72339e0af917399600eba6dd4e6cdacac2aefbb48b6f37abc34737
arm64v1mingw=b05a3f4a0a72339e0af917399600eba6dd4e6cdacac2aefbb48b6f37abc34737
arm64v1linux=82d468a664204ef632e1d6fb85e05069a628f10c49afd63e258aad26e1ff82a2
arm64v1musl=82d468a664204ef632e1d6fb85e05069a628f10c49afd63e258aad26e1ff82a2
arm64v1glibc=82d468a664204ef632e1d6fb85e05069a628f10c49afd63e258aad26e1ff82a2
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
