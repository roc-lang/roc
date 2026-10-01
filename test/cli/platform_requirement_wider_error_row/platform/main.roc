platform ""
    requires { main! : List(Str) => Try({}, [Exit(I32), Other, ..]) }
    exposes []
    packages {}
    provides { "roc_main": main_for_host! }

main_for_host! : List(Str) => I32
main_for_host! = |args|
    match main!(args) {
        Ok({}) => 0
        Err(Exit(code)) => code
        Err(Other) => 2
        Err(_) => 1
    }
