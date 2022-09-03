app "static-site"
    packages { pf: "platform/main.roc" }
    imports [
        pf.Html.{
            html,
            head,
            body,
            div,
            text,
            meta,
            title,
            link,
        }, # script, p, ul, li, lang,  },
        pf.Html.Attributes.{
            httpEquiv,
            content,
            href,
            rel,
            color,
            lang,
            media,
            type,
        },
    ]
    provides [transformFileContent] to pf

transformFileContent : Str -> Str
transformFileContent = \markdownHtmlText ->
    markdownHtmlText
    |> view
    |> Html.render

view : Str -> Html.Node
view = \markdownHtmlText ->
    html [] [
        head [] [
            meta [httpEquiv "content-type", content "text/html; charset=utf-8"] [],
            title [] [text "Daring Fireball: Markdown"],
            link [rel "apple-touch-icon-precomposed", href "/graphics/apple-touch-icon.png"] [],
            link [rel "shortcut icon", href "/graphics/favicon.ico?v=005"] [],
            link [rel "mask-icon", href "/graphics/dfstar.svg", color "#4a525"] [],
            link [rel "stylesheet", type "text/css", media "screen", href "/css/fireball_screen.css?v1.2022-08-01"] [],
            link [rel "stylesheet", type "text/css", media "screen", href "/css/ie_sucks.php"] [],
            link [rel "stylesheet", type "text/css", media "print", href "/css/fireball_print.css?v01"] [],
            link [rel "alternate", type "application/atom+xml", href "/feeds/main"] [],
            link [rel "alternate", type "application/json", href "/feeds/json"] [],
        ],
        body [] [
            div [] [
                # For now there is no escaping for `text` so we can cheekily insert the HTML from the Markdown file
                text markdownHtmlText,
            ],
        ],
    ]
