platform "echo-in-rust"
    requires {} { main : _ }
    exposes []
    packages {}
    provides [mainForHost]

# mainForHost : [StdoutWrite Str (({} -> Op) as Fx0), StderrWrite Str (({} -> Op) as Fx1), Done] as Op
mainForHost : [StdoutWrite Str ({} -> Op), StderrWrite Str ({} -> Op), Done] as Op
mainForHost = main

# mainForHost : { x: Str, y: {} -> Str }
# mainForHost =
#     y = "foo"
#
#     when main is
#         _ -> { x: "bar", y: \{} -> y }
