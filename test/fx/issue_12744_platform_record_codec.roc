app [Model, Msg, init, show] { pf: platform "./platform/requirement_codecs.roc" }

Model : { d : U64 }

Msg : [NoOp]

init : Str -> (Model, Msg)
init = |d| (Json.parse("{\"d\":${d}}") ?? { d: 0 }, NoOp)

show : Model -> Str
show = |model| model.d.to_str()
