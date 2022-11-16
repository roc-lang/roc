platform "echo-in-rust"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

Op : [StdoutWrite Str ({} -> Op), StderrWrite Str ({} -> Op), Done]

mainForHost : Op
mainForHost = main
