platform "echo-in-rust"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

Op : [StdoutWrite Str, StderrWrite Str]

mainForHost : Op
mainForHost = main
