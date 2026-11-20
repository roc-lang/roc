# META
~~~ini
description=Nested tag pattern matching with Err(Exit(code))
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

Error : [Exit(I64), NotFound]
Result : [Ok(I64), Err(Error)]

extract_code : Result -> I64
extract_code = |result|
    match result {
        Ok(n) => n
        Err(Exit(code)) => code
        Err(_) => -1
    }

main = Str.inspect(extract_code(Err(Exit(42))))
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
extract_code = |result| match result {
	Ok(n) => n
	Err(Exit(code)) => code
	Err(_) => -1
}
main = inspect(extract_code(Err(Exit(42))))

~~~
# DEV OUTPUT
~~~ini
x64mac=1a0d1d1c598f81c3b48977a0c9a5239963d4aa4f530114a74b041a46284e6bb2
x64win=4263c203317e3b731640c27d8b735ab85577ef779e5ab3ea95ebbe2fa30cff88
x64mingw=4263c203317e3b731640c27d8b735ab85577ef779e5ab3ea95ebbe2fa30cff88
x64freebsd=d8b0557d934fcb96bb3c28141d95b893c592345baa31bcc0cf1433e07f7639d4
x64openbsd=e4cfd36c38e799cd071b0c17bae6c4fdf71c0b6b36ee7ae00f46204920b133e4
x64netbsd=9ae600a725fdab4e3e41d7d38b323f9e870f0b3c4107c34b8b61bf55d1b263a4
x64musl=9ae600a725fdab4e3e41d7d38b323f9e870f0b3c4107c34b8b61bf55d1b263a4
x64glibc=9ae600a725fdab4e3e41d7d38b323f9e870f0b3c4107c34b8b61bf55d1b263a4
x64linux=9ae600a725fdab4e3e41d7d38b323f9e870f0b3c4107c34b8b61bf55d1b263a4
x64elf=9ae600a725fdab4e3e41d7d38b323f9e870f0b3c4107c34b8b61bf55d1b263a4
x64v1mac=1a0d1d1c598f81c3b48977a0c9a5239963d4aa4f530114a74b041a46284e6bb2
x64v1win=4263c203317e3b731640c27d8b735ab85577ef779e5ab3ea95ebbe2fa30cff88
x64v1mingw=4263c203317e3b731640c27d8b735ab85577ef779e5ab3ea95ebbe2fa30cff88
x64v1freebsd=d8b0557d934fcb96bb3c28141d95b893c592345baa31bcc0cf1433e07f7639d4
x64v1openbsd=e4cfd36c38e799cd071b0c17bae6c4fdf71c0b6b36ee7ae00f46204920b133e4
x64v1netbsd=9ae600a725fdab4e3e41d7d38b323f9e870f0b3c4107c34b8b61bf55d1b263a4
x64v1musl=9ae600a725fdab4e3e41d7d38b323f9e870f0b3c4107c34b8b61bf55d1b263a4
x64v1glibc=9ae600a725fdab4e3e41d7d38b323f9e870f0b3c4107c34b8b61bf55d1b263a4
x64v1linux=9ae600a725fdab4e3e41d7d38b323f9e870f0b3c4107c34b8b61bf55d1b263a4
x64v1elf=9ae600a725fdab4e3e41d7d38b323f9e870f0b3c4107c34b8b61bf55d1b263a4
arm64mac=dd9fd5a3748baa7162a7e0f2b0316fe15d1973324bd773edc29c591a3f1fd30d
arm64win=b6e32ef41ec195385b2179bbfb891215536a6848d2d8a4e21033f83a5461e10b
arm64mingw=b6e32ef41ec195385b2179bbfb891215536a6848d2d8a4e21033f83a5461e10b
arm64linux=7f5be17ca682567a03cc592615f03aa3c612301d2213a03f79338ceaa01d64ab
arm64musl=7f5be17ca682567a03cc592615f03aa3c612301d2213a03f79338ceaa01d64ab
arm64glibc=7f5be17ca682567a03cc592615f03aa3c612301d2213a03f79338ceaa01d64ab
arm64v1win=b6e32ef41ec195385b2179bbfb891215536a6848d2d8a4e21033f83a5461e10b
arm64v1mingw=b6e32ef41ec195385b2179bbfb891215536a6848d2d8a4e21033f83a5461e10b
arm64v1linux=7f5be17ca682567a03cc592615f03aa3c612301d2213a03f79338ceaa01d64ab
arm64v1musl=7f5be17ca682567a03cc592615f03aa3c612301d2213a03f79338ceaa01d64ab
arm64v1glibc=7f5be17ca682567a03cc592615f03aa3c612301d2213a03f79338ceaa01d64ab
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
