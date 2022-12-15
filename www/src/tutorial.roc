app "roc-tutorial"
    packages { pf: "../../examples/static-site-gen/platform/main.roc" }    
    imports [
        pf.Html.{ html, head, body, div, main, p, section, h1, label, ol, input, text, nav, a, li, link, meta },
        pf.Html.Attributes.{ httpEquiv, content, name, for, id, type, placeholder, href, rel, lang, class, title },
    ]
    provides [transformFileContent] to pf

transformFileContent : Str, Str -> Str
transformFileContent = \_, htmlContent ->
    Html.render (view htmlContent)

view : Str -> Html.Node
view = \htmlContent ->
    html [lang "en"] [
        head [] [
            meta [httpEquiv "content-type", content "text/html; charset=utf-8"] [],
            Html.title [] [text "Roc Tutorial"],
            link [rel "stylesheet", href "site.css"] [],
        ],
        body [] [
            viewNavbar,
            main [] [
                viewTutorialStart,
                text htmlContent,
            ],
        ],
    ]

viewNavbar : Html.Node
viewNavbar =
    div [id "top-bar"] [
        nav [] [
            a [class "home-link", href "/", title "The Roc Programming Language"] [text "roc"],
            div [id "top-bar-links"] [
                a [href "/tutorial"] [text "tutorial"],
                a [href "/install"] [text "install"],
                a [href "/repl"] [text "repl"],
                a [href "/builtins/Bool"] [text "docs"],
            ],
        ],
    ]

viewTutorialStart : Html.Node
viewTutorialStart =
    div [id "tutorial-start"] [
        input [id "tutorial-toc-toggle", name "tutorial-toc-toggle", type "checkbox"] [],
        nav [id "tutorial-toc"] [
            label [id "close-tutorial-toc", for "tutorial-toc-toggle"] [text "close"],
            input [id "toc-search", type "text", placeholder "Search"] [],
            ol [] tocLinks,
        ],
        tutorialIntro,
    ]

tocLinks =
    { tag, value } <- List.map [
        { tag: "#installation", value: "Installation" },
        { tag: "#strings-and-numbers", value: "Strings and Numbers" },
        { tag: "#building-an-application", value: "Building an Application" },
        { tag: "#defining-functions", value: "Defining Functions" },
        { tag: "#if-then-else", value: "if-then-else" },
        { tag: "#records", value: "Records" },
        { tag: "#debugging", value: "Debugging" },
        { tag: "#tags", value: "Tags &amp; Pattern Matching" },
        { tag: "#booleans", value: "Booleans" },
        { tag: "#lists", value: "Lists" },
        { tag: "#types", value: "Types" },
        { tag: "#crashing", value: "Crashing" },
        { tag: "#tests-and-expectations", value: "Tests and Expectations" },
        { tag: "#modules", value: "Modules" },
        { tag: "#tasks", value: "Tasks" },
        { tag: "#abilities", value: "Abilities" },
        { tag: "#appendix-advanced-concepts", value: "Advanced Concepts" },
        { tag: "#operator-desugaring-table", value: "Operator Desugaring Table" },
    ]

    li [] [
        a [href tag] [text value],
    ]

tutorialIntro =
    div [id "tutorial-intro"] [
        section [] [
            h1 [] [
                text "Tutorial",
                label [id "tutorial-toc-toggle-label", for "tutorial-toc-toggle"] [text "contents"],
            ],
            p [] [text "Welcome to Roc!"],
            p [] [text "This tutorial will teach you how to build Roc applications. Along the way, you'll learn how to write tests, use the REPL, and optionally annotate your types."],
        ],
    ]