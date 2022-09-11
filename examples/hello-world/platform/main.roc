platform "hello-world"
    requires {} { main : Str }
    exposes []
    packages {}
    imports []
    tests expectFx
    targets {
        x86_64-unknown-freebsd: "my_x64_freebsd_build_cmd",
        wasm32-unknown-unknown: "my_wasm_build_cmd",
    }
    provides [mainForHost]

mainForHost : Str
mainForHost = main
