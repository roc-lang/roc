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
x64mac=dacaad75e12afd6ce2d77fab97141f6429887c6729484ae7c65dad31bd7b8009
x64win=f9022ef6ebe80734c48441731ad759c3e1ecb8138e12faca0f18c70277de634d
x64mingw=f9022ef6ebe80734c48441731ad759c3e1ecb8138e12faca0f18c70277de634d
x64freebsd=a176cd8f9a5770ff7d99a9d897da2c33e2d168914435f957ab102abf180f2c2f
x64openbsd=58eb4a08fb6f4eeccd5383191f0c5a52cfd1bde2fad788a1bdf9eeff21f6cf4f
x64netbsd=0fbb000785de2d1c232a5487c885526ee2590efd55568e3682b3a7358d6c714f
x64musl=0fbb000785de2d1c232a5487c885526ee2590efd55568e3682b3a7358d6c714f
x64glibc=0fbb000785de2d1c232a5487c885526ee2590efd55568e3682b3a7358d6c714f
x64linux=0fbb000785de2d1c232a5487c885526ee2590efd55568e3682b3a7358d6c714f
x64elf=0fbb000785de2d1c232a5487c885526ee2590efd55568e3682b3a7358d6c714f
x64v1mac=dacaad75e12afd6ce2d77fab97141f6429887c6729484ae7c65dad31bd7b8009
x64v1win=f9022ef6ebe80734c48441731ad759c3e1ecb8138e12faca0f18c70277de634d
x64v1mingw=f9022ef6ebe80734c48441731ad759c3e1ecb8138e12faca0f18c70277de634d
x64v1freebsd=a176cd8f9a5770ff7d99a9d897da2c33e2d168914435f957ab102abf180f2c2f
x64v1openbsd=58eb4a08fb6f4eeccd5383191f0c5a52cfd1bde2fad788a1bdf9eeff21f6cf4f
x64v1netbsd=0fbb000785de2d1c232a5487c885526ee2590efd55568e3682b3a7358d6c714f
x64v1musl=0fbb000785de2d1c232a5487c885526ee2590efd55568e3682b3a7358d6c714f
x64v1glibc=0fbb000785de2d1c232a5487c885526ee2590efd55568e3682b3a7358d6c714f
x64v1linux=0fbb000785de2d1c232a5487c885526ee2590efd55568e3682b3a7358d6c714f
x64v1elf=0fbb000785de2d1c232a5487c885526ee2590efd55568e3682b3a7358d6c714f
arm64mac=4df4650ef65453e494bb80090eb592afa8039ad3c48b7dc1605c359cf88e3182
arm64win=f289ef8d3de2a58c894b962e3a836d95251be0121715ffac10eeffc99ee19c62
arm64mingw=f289ef8d3de2a58c894b962e3a836d95251be0121715ffac10eeffc99ee19c62
arm64linux=bcd722e08ef440d53562e50ed99fdb845e97f47397f142e066d3133bdb2add31
arm64musl=bcd722e08ef440d53562e50ed99fdb845e97f47397f142e066d3133bdb2add31
arm64glibc=bcd722e08ef440d53562e50ed99fdb845e97f47397f142e066d3133bdb2add31
arm64v1win=f289ef8d3de2a58c894b962e3a836d95251be0121715ffac10eeffc99ee19c62
arm64v1mingw=f289ef8d3de2a58c894b962e3a836d95251be0121715ffac10eeffc99ee19c62
arm64v1linux=bcd722e08ef440d53562e50ed99fdb845e97f47397f142e066d3133bdb2add31
arm64v1musl=bcd722e08ef440d53562e50ed99fdb845e97f47397f142e066d3133bdb2add31
arm64v1glibc=bcd722e08ef440d53562e50ed99fdb845e97f47397f142e066d3133bdb2add31
arm32linux=NOT_IMPLEMENTED
arm32musl=NOT_IMPLEMENTED
wasm32=NOT_IMPLEMENTED
wasm32v1=NOT_IMPLEMENTED
~~~
