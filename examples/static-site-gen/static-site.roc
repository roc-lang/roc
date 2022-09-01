app "static-site"
    packages { pf: "platform/main.roc" }
    imports [pf.Html.{ html, head, body, div, text }]
    provides [transformFileContent] to pf

transformFileContent : List U8 -> Result (List U8) Str
transformFileContent = \content ->
    when Str.fromUtf8 content is
        Err _ -> Err "Invalid UTF-8"
        Ok contentStr ->
            contentStr
            |> view
            |> Html.render
            |> Str.toUtf8
            |> Ok

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
