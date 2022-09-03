app "static-site"
    packages { pf: "platform/main.roc" }
    imports [pf.Html.{ html, head, body, div, text }]
    provides [transformFileContent] to pf

transformFileContent : Str -> Str
transformFileContent = \content ->
    content
    |> view
    |> Html.render

view : Str -> Html.Node
view = \content ->
    html [] [
        head [] [],
        body [] [
            div [] [
                text content,
            ],
        ],
    ]
