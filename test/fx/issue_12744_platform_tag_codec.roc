app [Model, Msg, init, show] { pf: platform "./platform/requirement_codecs.roc" }

Model : [A, B(U64)]

Msg : [NoOp]

init : Str -> (Model, Msg)
init = |d| (Json.parse("{\"B\":${d}}") ?? A, NoOp)

show : Model -> Str
show = |model|
    match model {
        A => "A"
        B(d) => d.to_str()
    }
