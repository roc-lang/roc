platform ""
    requires {} { main! : List(Str) => Try({}, [Exit(I8)]) }
    exposes []
    packages {}
    provides { "roc_main": main_for_host! }
    hosted {}
    targets: {}

main_for_host! : List(Str) => I8
main_for_host! = |args|
    match main!(args) {
        Ok({}) => 0
        Err(Exit(code)) => code
    }
