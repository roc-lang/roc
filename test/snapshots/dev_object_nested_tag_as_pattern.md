# META
~~~ini
description=Nested tag matching through as-pattern wrapper
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
        Err(Exit(code) as inner) =>
            match inner {
                Exit(_) => code
                _ => -2
            }
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
	Err(Exit(code) as inner) => match Exit(code) as inner {
		Exit(_) => code
		_ => -2
	}
	Err(_) => -1
}
main = inspect(extract_code(Err(Exit(42))))

~~~
# DEV OUTPUT
~~~ini
x64mac=6b3e2f6444568e0d88817c73cc467a3ef1cb0700e61b9398aba11f0322cd3919
x64win=8c5d80b85acaddf711276591305d30d93cf65b3935b338bece5877b6b210f3a1
x64mingw=8c5d80b85acaddf711276591305d30d93cf65b3935b338bece5877b6b210f3a1
x64freebsd=eebc88303e44d6ef95c858ca9db05735a55975ba12ca1d8e14434755a06e3e54
x64openbsd=e998eca92fb7b81958f8e4c55d2e584083d169dbd81e38b4522ebc8dc1dc1666
x64netbsd=fa96ac448c3107eb0828b9dea22f8a3586d21dfdac57cfde599efc20198b821e
x64musl=fa96ac448c3107eb0828b9dea22f8a3586d21dfdac57cfde599efc20198b821e
x64glibc=fa96ac448c3107eb0828b9dea22f8a3586d21dfdac57cfde599efc20198b821e
x64linux=fa96ac448c3107eb0828b9dea22f8a3586d21dfdac57cfde599efc20198b821e
x64elf=fa96ac448c3107eb0828b9dea22f8a3586d21dfdac57cfde599efc20198b821e
x64v1mac=6b3e2f6444568e0d88817c73cc467a3ef1cb0700e61b9398aba11f0322cd3919
x64v1win=8c5d80b85acaddf711276591305d30d93cf65b3935b338bece5877b6b210f3a1
x64v1mingw=8c5d80b85acaddf711276591305d30d93cf65b3935b338bece5877b6b210f3a1
x64v1freebsd=eebc88303e44d6ef95c858ca9db05735a55975ba12ca1d8e14434755a06e3e54
x64v1openbsd=e998eca92fb7b81958f8e4c55d2e584083d169dbd81e38b4522ebc8dc1dc1666
x64v1netbsd=fa96ac448c3107eb0828b9dea22f8a3586d21dfdac57cfde599efc20198b821e
x64v1musl=fa96ac448c3107eb0828b9dea22f8a3586d21dfdac57cfde599efc20198b821e
x64v1glibc=fa96ac448c3107eb0828b9dea22f8a3586d21dfdac57cfde599efc20198b821e
x64v1linux=fa96ac448c3107eb0828b9dea22f8a3586d21dfdac57cfde599efc20198b821e
x64v1elf=fa96ac448c3107eb0828b9dea22f8a3586d21dfdac57cfde599efc20198b821e
arm64mac=83829a1bcc8f6eac951e14e11aa4c677fffd0e42696cbdd629aeb4341760198e
arm64win=eff0e895ea34a70b5119e05da9f8eae54be537c3738253fd09aebc9b5db3f7c4
arm64mingw=eff0e895ea34a70b5119e05da9f8eae54be537c3738253fd09aebc9b5db3f7c4
arm64linux=de3faec21e393e9434cca9c84747e5275bb2003ce88cd2ffbf8cca8d2b0f7f7c
arm64musl=de3faec21e393e9434cca9c84747e5275bb2003ce88cd2ffbf8cca8d2b0f7f7c
arm64glibc=de3faec21e393e9434cca9c84747e5275bb2003ce88cd2ffbf8cca8d2b0f7f7c
arm64v1win=eff0e895ea34a70b5119e05da9f8eae54be537c3738253fd09aebc9b5db3f7c4
arm64v1mingw=eff0e895ea34a70b5119e05da9f8eae54be537c3738253fd09aebc9b5db3f7c4
arm64v1linux=de3faec21e393e9434cca9c84747e5275bb2003ce88cd2ffbf8cca8d2b0f7f7c
arm64v1musl=de3faec21e393e9434cca9c84747e5275bb2003ce88cd2ffbf8cca8d2b0f7f7c
arm64v1glibc=de3faec21e393e9434cca9c84747e5275bb2003ce88cd2ffbf8cca8d2b0f7f7c
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
