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
            link,
            script,
            img,
            a,
            p,
            br,
            input,
            form,
            ul,
            li,
            h1,
            strong,
        },
        pf.Html.Attributes.{
            httpEquiv,
            content,
            href,
            rel,
            color,
            lang,
            media,
            type,
            src,
            id,
            alt,
            class,
            # input, # importing from wrong module causes a crash, should have a error message
            name,
            value,
            style,
            method,
            action,
            height,
            title,
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
    html [lang "en"] [
        head [] [
            meta [httpEquiv "content-type", content "text/html; charset=utf-8"] [],
            Html.title [] [text "Daring Fireball: Markdown"],
            link [rel "apple-touch-icon-precomposed", href "/graphics/apple-touch-icon.png"] [],
            link [rel "shortcut icon", href "/graphics/favicon.ico?v=005"] [],
            link [rel "mask-icon", href "/graphics/dfstar.svg", color "#4a525"] [],
            link [rel "stylesheet", type "text/css", media "screen", href "/css/fireball_screen.css?v1.2022-08-01"] [],
            link [rel "stylesheet", type "text/css", media "screen", href "/css/ie_sucks.php"] [],
            link [rel "stylesheet", type "text/css", media "print", href "/css/fireball_print.css?v01"] [],
            link [rel "alternate", type "application/atom+xml", href "/feeds/main"] [],
            link [rel "alternate", type "application/json", href "/feeds/json"] [],
            script [src "/js/js-global/FancyZoom.js"] [],
            script [src "/js/js-global/FancyZoomHTML.js"] [],
        ],
        body [] [
            div [id "Box"] [
                div [id "Banner"] [
                    a
                        [
                            href "/",
                            title "Daring Fireball: Home",
                        ]
                        [
                            img
                                [
                                    src "/graphics/logos/",
                                    alt "Daring Fireball",
                                    height "56",
                                ]
                                [],
                        ],
                ],
                div [id "Sidebar"] [
                    p [] [
                        text "By",
                        strong
                            []
                            [text "John Gruber"],
                    ],
                    ul [] [
                        li [] [
                            a
                                [
                                    href "/archive/",
                                    title "Previous articles.",
                                ]
                                [text "Archive"],
                        ],
                        li
                            []
                            [],
                        li [] [
                            a
                                [
                                    href "/thetalkshow/",
                                    title "America’s favorite 3-star podcast.",
                                ]
                                [text "The Talk Show"],
                        ],
                        li [] [
                            a
                                [
                                    href "https://dithering.fm/",
                                    title "Three episodes per week, 15 minutes per episode. Not a minute less, not a minute more.",
                                ]
                                [text "Dithering"],
                        ],
                        li [] [
                            a
                                [
                                    href "/projects/",
                                    title "Software projects, including SmartyPants and Markdown.",
                                ]
                                [text "Projects"],
                        ],
                        li [] [
                            a
                                [
                                    href "/contact/",
                                    title "How to send email or feedback regarding Daring Fireball.",
                                ]
                                [text "Contact"],
                        ],
                        li [] [
                            a
                                [
                                    href "/colophon/",
                                    title "About this site and the tools used to produce it.",
                                ]
                                [text "Colophon"],
                        ],
                        li [] [
                            a
                                [
                                    href "/feeds/",
                                ]
                                [text "RSS Feed"],
                        ],
                        li [] [
                            a
                                [
                                    href "https://twitter.com/daringfireball",
                                ]
                                [text "Twitter"],
                        ],
                        li [] [
                            a
                                [
                                    href "/feeds/sponsors/",
                                ]
                                [text "Sponsorship"],
                        ],
                    ],
                    div [id "SidebarMartini"] [
                        a
                            [
                                href "https://www.statushero.com/?utm_source=df&utm_medium=paid&utm_content=display",
                            ]
                            [
                                img
                                    [
                                        alt "Status Hero",
                                        src "/martini/images/statushero-c.png",
                                        height "90",
                                    ]
                                    [],
                            ],
                        p [] [
                            text "Automatic async daily check‑in software for teams everywhere.",
                            a
                                [
                                    href "https://www.statushero.com/?utm_source=df&utm_medium=paid&utm_content=display",
                                ]
                                [text "Learn more"],
                            text ".",
                        ],
                    ],
                ],
                div [id "Main"] [
                    div [class "article"] [
                        h1
                            []
                            [text "Markdown"],
                        ul [id "ProjectSubmenu"] [
                            li [] [
                                a
                                    [
                                        class "selected",
                                        title "Markdown Project Page",
                                    ]
                                    [text "Main"],
                            ],
                            li [] [
                                a
                                    [
                                        href "/projects/markdown/basics",
                                        title "Markdown Basics",
                                    ]
                                    [text "Basics"],
                            ],
                            li [] [
                                a
                                    [
                                        href "/projects/markdown/syntax",
                                        title "Markdown Syntax Documentation",
                                    ]
                                    [text "Syntax"],
                            ],
                            li [] [
                                a
                                    [
                                        href "/projects/markdown/license",
                                        title "Pricing and License Information",
                                    ]
                                    [text "License"],
                            ],
                            li [] [
                                a
                                    [
                                        href "/projects/markdown/dingus",
                                        title "Online Markdown Web Form",
                                    ]
                                    [text "Dingus"],
                            ],
                        ],
                        # For now there is no escaping for `text` so we can cheekily insert the HTML from the Markdown file
                        text markdownHtmlText,

                    ],
                    div [id "Footer"] [
                        form
                            [
                                id "SiteSearch",
                                action "https://daringfireball.net/search",
                                method "get",
                                style "margin-bottom:2.5em",
                            ]
                            [
                                div [] [
                                    input
                                        [
                                            name "q",
                                            type "text",
                                            value "",
                                            style "margin-right:8px; width:66%",
                                        ]
                                        [],
                                    input
                                        [
                                            type "submit",
                                            value "Search",
                                        ]
                                        [],
                                ],
                            ],
                        p [class "smallprint"] [
                            a
                                [
                                    href "/preferences/",
                                    title "Customize the font size and presentation options for this web site.",
                                ]
                                [text "Display Preferences"],
                            br
                                []
                                [],
                            br
                                []
                                [],
                            text "Copyright © 2002–2022 The Daring Fireball Company LLC.",
                        ],
                    ],
                ],
            ],
        ],
    ]
