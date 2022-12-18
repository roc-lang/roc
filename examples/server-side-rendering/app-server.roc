app "app-server"
    packages { pf: "platform/server-side.roc" }
    imports [
        pf.Html.{ App, Html, html, head, body, div, text, h1 },
    ]
    provides [app] to pf

State : {
    answer : U32,
}

app : App State State
app = {
    init: \state -> state,
    render,
    wasmUrl: "assets/app.wasm",
}

# This function must be identical in both server and client.
# Of course, it should be in a separate .roc file imported into both,
# but that has not been implemented yet. Currently apps are single-file
# and can only import from the platform!
render : State -> Html State
render = \state ->
    num = Num.toStr state.answer

    html [] [
        head [] [],
        body [] [
            h1 [] [text "The app"],
            div [] [text "The answer is \(num)"],
        ],
    ]

expect Html.renderStatic (Html.translateStatic (render { answer: 42 })) == ""
