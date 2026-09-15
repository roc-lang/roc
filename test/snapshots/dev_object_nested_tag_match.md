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
x64mac=10c831721e4c757ccf1b07e64379c929eb78af5f8847442cb7d2ece90ac48c2f
x64win=45134acd4bebd34e5d0e72ee6a69007b79afad4828b7a4c27a633fcd9e801379
x64mingw=45134acd4bebd34e5d0e72ee6a69007b79afad4828b7a4c27a633fcd9e801379
x64freebsd=dbe5a34143655e5b805ccb48b0bab94ba8136f1198c8a4b5d503476171cf19ab
x64openbsd=85c6aaeabc5cb356c50dfd29c3d68fe96ba93655fb4328cd88bd3e73e8b9460f
x64netbsd=c8a23ab481b850d2613bbea2f9d2097d4e9ff45cd77f501eefd2dae64a8bdb09
x64musl=c8a23ab481b850d2613bbea2f9d2097d4e9ff45cd77f501eefd2dae64a8bdb09
x64glibc=c8a23ab481b850d2613bbea2f9d2097d4e9ff45cd77f501eefd2dae64a8bdb09
x64linux=c8a23ab481b850d2613bbea2f9d2097d4e9ff45cd77f501eefd2dae64a8bdb09
x64elf=c8a23ab481b850d2613bbea2f9d2097d4e9ff45cd77f501eefd2dae64a8bdb09
x64v1mac=10c831721e4c757ccf1b07e64379c929eb78af5f8847442cb7d2ece90ac48c2f
x64v1win=45134acd4bebd34e5d0e72ee6a69007b79afad4828b7a4c27a633fcd9e801379
x64v1mingw=45134acd4bebd34e5d0e72ee6a69007b79afad4828b7a4c27a633fcd9e801379
x64v1freebsd=dbe5a34143655e5b805ccb48b0bab94ba8136f1198c8a4b5d503476171cf19ab
x64v1openbsd=85c6aaeabc5cb356c50dfd29c3d68fe96ba93655fb4328cd88bd3e73e8b9460f
x64v1netbsd=c8a23ab481b850d2613bbea2f9d2097d4e9ff45cd77f501eefd2dae64a8bdb09
x64v1musl=c8a23ab481b850d2613bbea2f9d2097d4e9ff45cd77f501eefd2dae64a8bdb09
x64v1glibc=c8a23ab481b850d2613bbea2f9d2097d4e9ff45cd77f501eefd2dae64a8bdb09
x64v1linux=c8a23ab481b850d2613bbea2f9d2097d4e9ff45cd77f501eefd2dae64a8bdb09
x64v1elf=c8a23ab481b850d2613bbea2f9d2097d4e9ff45cd77f501eefd2dae64a8bdb09
arm64mac=2f314b05112a6314051b869991c78fd37f5d17a01748df9739759376a07c4aae
arm64win=0f21fd288ea1f7b890c87f6d2fa34355910f5e464041e53b34a1cca05f3812a2
arm64mingw=0f21fd288ea1f7b890c87f6d2fa34355910f5e464041e53b34a1cca05f3812a2
arm64linux=8eefed621a1b95e2c7d8a013395e23d63768d4155443775ec6e9b4d675114535
arm64musl=8eefed621a1b95e2c7d8a013395e23d63768d4155443775ec6e9b4d675114535
arm64glibc=8eefed621a1b95e2c7d8a013395e23d63768d4155443775ec6e9b4d675114535
arm64v1win=0f21fd288ea1f7b890c87f6d2fa34355910f5e464041e53b34a1cca05f3812a2
arm64v1mingw=0f21fd288ea1f7b890c87f6d2fa34355910f5e464041e53b34a1cca05f3812a2
arm64v1linux=8eefed621a1b95e2c7d8a013395e23d63768d4155443775ec6e9b4d675114535
arm64v1musl=8eefed621a1b95e2c7d8a013395e23d63768d4155443775ec6e9b4d675114535
arm64v1glibc=8eefed621a1b95e2c7d8a013395e23d63768d4155443775ec6e9b4d675114535
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
