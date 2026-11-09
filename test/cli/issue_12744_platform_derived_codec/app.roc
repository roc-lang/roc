app [Model, Msg, init] { pf: platform "./platform/main.roc" }

Model : { d : U64 }

Msg : [NoOp]

init : Str -> (Model, Msg)
init = |flags| (Json.parse(flags) ?? { d: 0 }, NoOp)
