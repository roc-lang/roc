app "app-server"
    packages { pf: "platform/server-side.roc" }
    imports [
        pf.Html.{ App, Html, html, head, body, div, text, h1 },
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

# This function must be identical in both server and client.
# Of course, it should be in a separate .roc file imported into both,
# but that has not been implemented yet. Currently apps are single-file
# and can only import from the platform!
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

expect Html.renderStatic (Html.translateStatic (render 42)) == ""
