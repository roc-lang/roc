interface ExampleApp
    exposes [exampleApp, State]

import pf.Html exposing [App, Html, html, head, body, div, text, h1]

State : {
    answer : U32,
}

exampleApp : App State State
exampleApp = {
    init,
    render,
    wasmUrl: "assets/example-client.wasm",
}

init = \result ->
    when result is
        Ok state -> state
        Err _ -> { answer: 0 }

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

expect
    Html.renderStatic (Html.translateStatic (render { answer: 42 }))
    ==
    "<!DOCTYPE html><html><head></head><body><h1>The app</h1><div>The answer is 42</div></body></html>"
