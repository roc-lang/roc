platform "false-interpreter"
    requires {} { main! : Str => () }
    exposes []
    packages {}
    imports []
    provides [main_for_host!]

main_for_host! : Str => ()
main_for_host! = \file -> main!(file)
