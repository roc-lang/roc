import Msg exposing [Msg]

View(model) := {
    title : Str,
    _ : {},
    model : Box(model),
    messages : List(Msg),
    lifecycle : [Ready, Waiting(Str), Failed({ code : I32, reason : Str })],
}
