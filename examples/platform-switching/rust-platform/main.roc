platform "echo-in-rust"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

Op : [StdoutWrite Str ({} -> Op), StderrWrite Str ({} -> Op), Done]


# mainForHost : { bar: Str, foo: I64 -> I64 }
# mainForHost = { bar: main, foo: \x -> x }

mainForHost : Op
mainForHost = main
