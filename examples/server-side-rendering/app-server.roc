app "app-server"
    packages { pf: "platform/server-side.roc" }
    imports [
        pf.Html.{ App, Html, html, head, body, div, text, h1, table, tr, td, th },
    ]
    provides [app] to pf

# FIXME: compiler doesn't like record types here!
# Caused by recursive types inside the platform that trigger a type checker bug
State : U32

app : App State State
app = {
    init: \state -> state,
    render,
    wasmUrl: "assets/app.wasm",
}

render : State -> Html State
render = \state ->
    num = Num.toStr state

    html [] [
        head [] [],
        body [] [
            h1 [] [text "The app"],
            div [] [text "Your number is \(num)"],
        ],
    ]
