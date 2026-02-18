platform ""
    requires {} { main! : () => Str }
    exposes []
    packages {}
    provides { main_for_host!: "main" }
    targets: {
        files: "targets/",
        static_lib: {
            wasm32: ["libhost.a", app],
        }
    }

main_for_host! : () => Str
main_for_host! = main!
