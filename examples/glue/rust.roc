app "glue-rust"
    packages { pf: "../../crates/glue/platform/main.roc" }
    imports []
    provides [main] to pf

glue : Str -> Str
glue = \input -> "\(input)!!"
