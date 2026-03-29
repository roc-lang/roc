platform ""
    requires {} { main! : () => Str }
    exposes []
    packages {}
    provides { main_for_host!: "main" }
    targets: {
        files: "targets/",
        exe: {
            wasm32: ["host.wasm", app],
        }
    }

main_for_host! : () => Str
main_for_host! = main!
