app "static-site"
    packages { pf: "../../../examples/static-site-gen/platform/main.roc" }    
    imports [
        pf.Html.{ html, head, body, footer, script, main, div, text, nav, a, link, meta },
        pf.Html.Attributes.{ content, name, id, href, rel, lang, class, title, charset, src },
    ]
    provides [transformFileContent] to pf

transformFileContent : Str, Str -> Str
transformFileContent = \relPath, htmlContent ->
    Html.render (view relPath htmlContent)

relPathToName : Str -> Str 
relPathToName = \relPath -> 
    when relPath is 
        "tutorial.html" -> "Roc Tutorial"
        "crash-reporting.html" -> "Design: Crash Reporting"
        "dbg-feature.html" -> "Design: Debugging"
        "design.html" -> "Design Blog"
        _ -> "Roc"

relPathToDescription : Str -> Str 
relPathToDescription = \relPath -> 
    when relPath is 
        "tutorial.html" -> "Learn how to use the Roc programming language."
        "crash-reporting.html" -> "A design article on crash reporting"
        "dbg-feature.html" -> "A design article on debugging"
        "design.html" -> "Contribute to the design of the Roc programming langugage"
        _ -> "A Roc blog post"

view : Str, Str -> Html.Node
view = \relPath, htmlContent ->
    html [lang "en"] [
        head [] [
            meta [charset "utf-8"] [],
            Html.title [] [text (relPathToName relPath)],
            meta [name "description", content (relPathToDescription relPath)] [],
            meta [name "viewport", content "width=device-width"] [],
            link [rel "stylesheet", href "/site.css"] [],
            link [rel "icon", href "/favicon.svg"] [],
        ],
        body [] [
            viewNavbar,
            main [] [
                text htmlContent,
            ],
            footer [] [
                text "Made by people who like to make nice things. © 2022"
            ]
        ],
        script [src "/site.js"] [],
    ]

viewNavbar : Html.Node
viewNavbar =
    div [id "top-bar"] [
        nav [] [
            a [class "home-link", href "/", title "The Roc Programming Language"] [text "roc"],
            div [id "top-bar-links"] [
                a [href "/tutorial"] [text "tutorial"],
                a [href "https://github.com/roc-lang/roc/tree/main/getting_started"] [text "install"],
                a [href "/repl"] [text "repl"],
                a [href "/builtins/Bool"] [text "docs"],
                a [href "/design.html"] [text "design"],
            ],
        ],
    ]
