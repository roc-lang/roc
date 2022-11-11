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

# This test should simply fail, but instead it actually panics!
#
# ── EXPECT PANICKED ───────────── examples/server-side-rendering/app-server.roc ─
# This expectation crashed while running:
# 31│  expect (Html.renderStatic (render 42)) == ""
#      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# The crash reported this message:
# Hit an erroneous type when creating a layout for `#UserApp.render`
# 1 failed and 0 passed in 1177 ms.
expect (Html.renderStatic (render 42)) == ""
