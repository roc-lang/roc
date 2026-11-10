# META
~~~ini
description=F32/F64 arithmetic, comparisons, and F64 to I64 conversion in host-called functions
type=dev_object
~~~
# SOURCE
## app.roc
~~~roc
app [main] { pf: platform "./platform.roc" }

main : F64, F32, I64 -> I64
main = |x, y, n| {
    scaled = x * 2.5 - I64.to_f64(n) / 3.0
    narrowed = F32.to_f64(y * 1.5 + 0.25)
    total = if scaled > narrowed { scaled - narrowed } else { narrowed + scaled }
    F64.to_i64_wrap(total)
}
~~~
## platform.roc
~~~roc
platform ""
    requires {} { main : F64, F32, I64 -> I64 }
    exposes []
    packages {}
    provides { "roc_main": main_for_host }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app] },
    }

main_for_host : F64, F32, I64 -> I64
main_for_host = main
~~~
# MONO
~~~roc
# platform
main_for_host = <required>

# app
main = |x, y, n| {
	scaled = x * 2.5 - to_f64(n) / 3
	narrowed = to_f64(y * 1.5 + 0.25)
	total = if (scaled > narrowed) {
		scaled - narrowed
	} else {
		narrowed + scaled
	}
	to_i64_wrap(total)
}

~~~
# DEV OUTPUT
~~~ini
x64mac=b80f8ac62964d4dd0cd7949801f29addd6b342f390eb6f58ebf085a6ee11a57c
x64win=b074a7a0e718372235ddb0bc0093e5710c8e5574951995ce2d066f0410c530c4
x64mingw=b074a7a0e718372235ddb0bc0093e5710c8e5574951995ce2d066f0410c530c4
x64freebsd=223ae77aa39e6c97075513d5ac7272c7763bc3a6cb911fd0de2945b25a3e811d
x64openbsd=6d24208ee5615adb08e5329481a06fbf85d68d72f8ad6e0f791196313da97b60
x64netbsd=e1c89cbe29b11f1afcff0113f8e3fb4665255da95da221e36cb1d095dc22551c
x64musl=e1c89cbe29b11f1afcff0113f8e3fb4665255da95da221e36cb1d095dc22551c
x64glibc=e1c89cbe29b11f1afcff0113f8e3fb4665255da95da221e36cb1d095dc22551c
x64linux=e1c89cbe29b11f1afcff0113f8e3fb4665255da95da221e36cb1d095dc22551c
x64elf=e1c89cbe29b11f1afcff0113f8e3fb4665255da95da221e36cb1d095dc22551c
x64v1mac=b80f8ac62964d4dd0cd7949801f29addd6b342f390eb6f58ebf085a6ee11a57c
x64v1win=b074a7a0e718372235ddb0bc0093e5710c8e5574951995ce2d066f0410c530c4
x64v1mingw=b074a7a0e718372235ddb0bc0093e5710c8e5574951995ce2d066f0410c530c4
x64v1freebsd=223ae77aa39e6c97075513d5ac7272c7763bc3a6cb911fd0de2945b25a3e811d
x64v1openbsd=6d24208ee5615adb08e5329481a06fbf85d68d72f8ad6e0f791196313da97b60
x64v1netbsd=e1c89cbe29b11f1afcff0113f8e3fb4665255da95da221e36cb1d095dc22551c
x64v1musl=e1c89cbe29b11f1afcff0113f8e3fb4665255da95da221e36cb1d095dc22551c
x64v1glibc=e1c89cbe29b11f1afcff0113f8e3fb4665255da95da221e36cb1d095dc22551c
x64v1linux=e1c89cbe29b11f1afcff0113f8e3fb4665255da95da221e36cb1d095dc22551c
x64v1elf=e1c89cbe29b11f1afcff0113f8e3fb4665255da95da221e36cb1d095dc22551c
arm64mac=ab59a92e919fc87a620b07daa8b9db163749f0dcad855219e6441c96a8c8ae94
arm64win=c360b9e4f62351ad0d238973ee53ab3ad9e13740d5d95c24b8ea2fd884b58311
arm64mingw=c360b9e4f62351ad0d238973ee53ab3ad9e13740d5d95c24b8ea2fd884b58311
arm64linux=ad2a4367c15155934e7504fcc4ca0fedbf6cc1927a0addc4f0564d0b97da2734
arm64musl=ad2a4367c15155934e7504fcc4ca0fedbf6cc1927a0addc4f0564d0b97da2734
arm64glibc=ad2a4367c15155934e7504fcc4ca0fedbf6cc1927a0addc4f0564d0b97da2734
arm64v1win=c360b9e4f62351ad0d238973ee53ab3ad9e13740d5d95c24b8ea2fd884b58311
arm64v1mingw=c360b9e4f62351ad0d238973ee53ab3ad9e13740d5d95c24b8ea2fd884b58311
arm64v1linux=ad2a4367c15155934e7504fcc4ca0fedbf6cc1927a0addc4f0564d0b97da2734
arm64v1musl=ad2a4367c15155934e7504fcc4ca0fedbf6cc1927a0addc4f0564d0b97da2734
arm64v1glibc=ad2a4367c15155934e7504fcc4ca0fedbf6cc1927a0addc4f0564d0b97da2734
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
