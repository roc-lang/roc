platform "echo-in-rust"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [mainForHost]


mainForHost : [StdoutWrite Str (({} -> Op) as Fx1), StderrWrite Str (({} -> Op) as Fx2), Done] as Op
mainForHost = main

# mainForHost : { x: Str, y: {} -> Str }
# mainForHost = 
#     y = "foo"
# 
#     when main is
#         _ -> { x: "bar", y: \{} -> y }
