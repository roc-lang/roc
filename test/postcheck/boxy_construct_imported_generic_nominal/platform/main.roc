platform ""
    requires {} { main! : List(Str) => Try({}, [Exit(I32), ..]) }
    exposes []
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {}
    targets: {
        inputs_dir: "targets/",
        x64musl: { inputs: [app], output: Archive },
        arm64musl: { inputs: [app], output: Archive },
    }

main_for_host! : List(Str) => I32
main_for_host! = |args|
    match main!(args) {
        Ok({}) => 0
        Err(Exit(code)) => code
        Err(_) => 1
    }
