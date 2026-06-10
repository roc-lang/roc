platform ""
    requires {} { main! : () => Str }
    exposes []
    packages {}
    provides { main_for_host!: "main" }
    targets: {
        files: "../platform/targets/",
        static_lib: {
            wasm32: { files: ["host.wasm", app] },
        }
    }

main_for_host! : () => Str
main_for_host! = main!
