platform ""
    requires {} { main! : () => List(I32) }
    exposes []
    packages {}
    provides { main_for_host!: "main" }
    targets: {
        files: "targets/",
        exe: { arm64mac: ["libhost.a", app], x64mac: ["libhost.a", app] }
    }

main_for_host! : () => List(I32)
main_for_host! = || main!()
