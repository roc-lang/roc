platform "echo-in-rust"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

# main_for_host : [StdoutWrite Str (({} -> Op) as Fx0), StderrWrite Str (({} -> Op) as Fx1), Done] as Op
main_for_host : [StdoutWrite Str ({} -> Op), StderrWrite Str ({} -> Op), Done] as Op
main_for_host = main

# main_for_host : { x: Str, y: {} -> Str }
# main_for_host =
#     y = "foo"
#
#     when main is
#         _ -> { x: "bar", y: \{} -> y }
