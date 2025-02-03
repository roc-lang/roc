app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

main! = |_|
    if Bool.true then
        return Err(Exit(1,"This is a test error message"))
    else
        Ok({})
