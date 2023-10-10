app "roc-tutorial"
    packages { pf: "../../../examples/static-site-gen/platform/main.roc" }
    imports [
        pf.Html.{ html, head, body, footer, script, div, main, p, section, h1, h2, label, ol, input, text, nav, a, li, link, meta },
        pf.Html.Attributes.{ content, name, for, id, type, href, rel, lang, class, title, charset, src },
    ]
    provides [transformFileContent] to pf

transformFileContent : Str, Str -> Str
transformFileContent = \_, htmlContent ->
    Html.render (view htmlContent)

view : Str -> Html.Node
view = \htmlContent ->
    html [lang "en"] [
        head [] [
            meta [charset "utf-8"] [],
            Html.title [] [text "Roc Tutorial"],
            meta [name "description", content "Learn how to use the Roc programming language."] [],
            meta [name "viewport", content "width=device-width"] [],
            link [rel "stylesheet", href "/site.css"] [],
            link [rel "icon", href "/favicon.svg"] [],
        ],
        body [] [
            viewNavbar,
            main [] [
                viewTutorialStart,
                text htmlContent,
            ],
            footer [] [
                text "Made by people who like to make nice things."
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
                a [href "/builtins"] [text "docs"],
            ],
        ],
    ]

viewTutorialStart : Html.Node
viewTutorialStart =
    div [id "tutorial-start"] [
        input [id "tutorial-toc-toggle", name "tutorial-toc-toggle", type "checkbox"] [],
        nav [id "tutorial-toc"] [
            label [id "close-tutorial-toc", for "tutorial-toc-toggle"] [text "close"],
            # TODO fix search: input [id "toc-search", type "text", placeholder "Search"] [],
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
            { tag: "#debugging", value: "Debugging" },
            { tag: "#records", value: "Records" },
            { tag: "#tags", value: "Tags &amp; Pattern Matching" },
            { tag: "#booleans", value: "Booleans" },
            { tag: "#lists", value: "Lists" },
            { tag: "#types", value: "Types" },
            { tag: "#numeric-types", value: "Numeric Types" },
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
            p [] [text "This tutorial will teach you how to build Roc applications. Along the way, you'll learn how to write tests, use the REPL, and more!"],
        ],
        section [] [
            h2 [id "installation"] [
                a [href "#installation"] [text "Installation"],
            ],
            p [] [
                text "Roc doesn’t have a numbered release or an installer yet, but you can follow the install instructions for your OS",
                a [href "https://github.com/roc-lang/roc/tree/main/getting_started#installation"] [text " here "],
                text ". If you get stuck, friendly people will be happy to help if you open a topic in",
                a [href "https://roc.zulipchat.com/#narrow/stream/231634-beginners"] [text " #beginners "],
                text "on",
                a [href "https://roc.zulipchat.com/"] [text " Roc Zulip Chat "],
                text "and ask for assistance!",
            ],
        ],
    ]
