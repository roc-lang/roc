platform "echo-in-rust"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : {} -> [StdoutWrite Str ({} -> Op), StderrWrite Str ({} -> Op), Done] as Op
mainForHost = \x -> main x
