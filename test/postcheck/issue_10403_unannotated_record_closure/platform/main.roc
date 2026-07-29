platform ""
    requires {} {
        program : {
            run : Str -> Str,
        },
    }
    exposes []
    packages {}
    provides {
        "roc_run": run_for_host,
    }
    targets: {
        inputs_dir: "targets/",
        x64glibc: { inputs: [app], output: Archive },
        arm64mac: { inputs: [app], output: Archive },
        x64win: { inputs: [app], output: Archive },
    }

run_for_host : Str -> Str
run_for_host = |s| (program.run)(s)
