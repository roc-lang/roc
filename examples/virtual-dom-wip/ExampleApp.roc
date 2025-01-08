module [example_app, State]

import pf.Html exposing [App, Html, html, head, body, div, text, h1]

State : {
    answer : U32,
}

example_app : App State State
example_app = {
    init,
    render,
    wasm_url: "assets/example-client.wasm",
}

init = \result ->
    when result is
        Ok(state) -> state
        Err(_) -> { answer: 0 }

render : State -> Html State
render = \state ->
    num = Num.to_str(state.answer)

    html([], [
        head([], []),
        body([], [
            h1([], [text("The app")]),
            div([], [text("The answer is $(num)")]),
        ]),
    ])

expect
    Html.render_static(Html.translate_static(render({ answer: 42 })))
    ==
    "<!DOCTYPE html><html><head></head><body><h1>The app</h1><div>The answer is 42</div></body></html>"
