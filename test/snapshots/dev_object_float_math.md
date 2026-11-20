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
x64mac=56b7426491be144c87e85c488d861cd89784a5db7af944351a44825318d66adc
x64win=29d3c9b582accee4972cb2af5b4a2980bcd0c87c080b3b1f77cab3d0452473d5
x64mingw=29d3c9b582accee4972cb2af5b4a2980bcd0c87c080b3b1f77cab3d0452473d5
x64freebsd=640dff1697015b228f61c7e8ee6e8bd7b04296ee9e3c039197e7f2134d062a68
x64openbsd=f7dc5ce3e996c5197dac19cb3f53d2210ddc38480bcd0ad987e2be34587a1f36
x64netbsd=f3459302b7c14b4272f536748f9c53034b18e3b39f64c3648f8dc7b13f84b3a3
x64musl=f3459302b7c14b4272f536748f9c53034b18e3b39f64c3648f8dc7b13f84b3a3
x64glibc=f3459302b7c14b4272f536748f9c53034b18e3b39f64c3648f8dc7b13f84b3a3
x64linux=f3459302b7c14b4272f536748f9c53034b18e3b39f64c3648f8dc7b13f84b3a3
x64elf=f3459302b7c14b4272f536748f9c53034b18e3b39f64c3648f8dc7b13f84b3a3
x64v1mac=56b7426491be144c87e85c488d861cd89784a5db7af944351a44825318d66adc
x64v1win=29d3c9b582accee4972cb2af5b4a2980bcd0c87c080b3b1f77cab3d0452473d5
x64v1mingw=29d3c9b582accee4972cb2af5b4a2980bcd0c87c080b3b1f77cab3d0452473d5
x64v1freebsd=640dff1697015b228f61c7e8ee6e8bd7b04296ee9e3c039197e7f2134d062a68
x64v1openbsd=f7dc5ce3e996c5197dac19cb3f53d2210ddc38480bcd0ad987e2be34587a1f36
x64v1netbsd=f3459302b7c14b4272f536748f9c53034b18e3b39f64c3648f8dc7b13f84b3a3
x64v1musl=f3459302b7c14b4272f536748f9c53034b18e3b39f64c3648f8dc7b13f84b3a3
x64v1glibc=f3459302b7c14b4272f536748f9c53034b18e3b39f64c3648f8dc7b13f84b3a3
x64v1linux=f3459302b7c14b4272f536748f9c53034b18e3b39f64c3648f8dc7b13f84b3a3
x64v1elf=f3459302b7c14b4272f536748f9c53034b18e3b39f64c3648f8dc7b13f84b3a3
arm64mac=456ea152dca0b9c7e598055fe1f11a3ba8624210ac72415abb96da090f7fce16
arm64win=c6c3a273c676ddae813331dd7a88b9807b22d19c67deb7580fd93e724274f91d
arm64mingw=c6c3a273c676ddae813331dd7a88b9807b22d19c67deb7580fd93e724274f91d
arm64linux=347770cc3616cccbd3d4f7627c568776377e23844e3e3b19aaaa205014c2c249
arm64musl=347770cc3616cccbd3d4f7627c568776377e23844e3e3b19aaaa205014c2c249
arm64glibc=347770cc3616cccbd3d4f7627c568776377e23844e3e3b19aaaa205014c2c249
arm64v1win=c6c3a273c676ddae813331dd7a88b9807b22d19c67deb7580fd93e724274f91d
arm64v1mingw=c6c3a273c676ddae813331dd7a88b9807b22d19c67deb7580fd93e724274f91d
arm64v1linux=347770cc3616cccbd3d4f7627c568776377e23844e3e3b19aaaa205014c2c249
arm64v1musl=347770cc3616cccbd3d4f7627c568776377e23844e3e3b19aaaa205014c2c249
arm64v1glibc=347770cc3616cccbd3d4f7627c568776377e23844e3e3b19aaaa205014c2c249
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
