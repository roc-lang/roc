# Repro for https://github.com/roc-lang/roc/issues/10670
platform "blub"
    requires {
        main : U8
    }
    exposes []
    packages {}
    provides {
        "roc_main": main_for_host,
    }
    targets: {
        inputs_dir: "targets/",
        x64musl: { inputs: [app], output: Archive },
    }

main_for_host : () -> U8
main_for_host = || main
