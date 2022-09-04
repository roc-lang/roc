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

transformFileContent : Str, Str -> Str
transformFileContent = \currentUrl, htmlContent ->
    view currentUrl htmlContent
    |> Html.render

view : Str, Str -> Html.Node
view = \currentUrl, htmlContent ->
    html [lang "en"] [
        head [] [
            meta [httpEquiv "content-type", content "text/html; charset=utf-8"] [],
            Html.title [] [text "Daring Fireball: Markdown"],
            link [rel "apple-touch-icon-precomposed", href "/graphics/apple-touch-icon.png"] [],
            link [rel "shortcut icon", href "/favicon.ico"] [],
            link [rel "mask-icon", href "/graphics/dfstar.svg", color "#4a525"] [],
            link [rel "stylesheet", type "text/css", media "screen", href "/css/fireball_screen.css"] [],
            link [rel "stylesheet", type "text/css", media "screen", href "/css/ie_sucks.css"] [],
            link [rel "stylesheet", type "text/css", media "print", href "/css/fireball_print.css"] [],
            # link [rel "alternate", type "application/atom+xml", href "/feeds/main"] [],
            # link [rel "alternate", type "application/json", href "/feeds/json"] [],
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
                                    src "/graphics/logo.png",
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
                                    href "https://daringfireball.net/archive/",
                                    title "Previous articles.",
                                ]
                                [text "Archive"],
                        ],
                        li [] [
                            a
                                [
                                    href "https://daringfireball.net/thetalkshow/",
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
                                    href "https://daringfireball.net/projects/",
                                    title "Software projects, including SmartyPants and Markdown.",
                                ]
                                [text "Projects"],
                        ],
                        li [] [
                            a
                                [
                                    href "https://daringfireball.net/contact/",
                                    title "How to send email or feedback regarding Daring Fireball.",
                                ]
                                [text "Contact"],
                        ],
                        li [] [
                            a
                                [
                                    href "https://daringfireball.net/colophon/",
                                    title "About this site and the tools used to produce it.",
                                ]
                                [text "Colophon"],
                        ],
                        li [] [
                            a
                                [
                                    href "https://daringfireball.net/feeds/",
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
                                    href "https://daringfireball.net/feeds/sponsors/",
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
                                        src "/graphics/statushero-c.png",
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
                        viewPageHeading currentUrl,
                        viewSubmenu currentUrl,
                        # For now `text` is not escaped so we can use it to insert HTML
                        # We'll probably want something more explicit in the long term
                        text htmlContent,
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
                                    href "https://daringfireball.net/preferences/",
                                    title "Customize the font size and presentation options for this web site.",
                                ]
                                [text "Display Preferences"],
                            br [] [],
                            br [] [],
                            text "Copyright © 2002–2022 The Daring Fireball Company LLC.",
                        ],
                    ],
                ],
            ],
        ],
    ]

NavLink : { linkUrl : Str, linkTitle : Str, linkText : Str }

navLinks : List NavLink
navLinks = [
    { linkUrl: "index.html", linkTitle: "Markdown Project Page", linkText: "Main" },
    { linkUrl: "basics.html", linkTitle: "Markdown Basics", linkText: "Basics" },
    { linkUrl: "syntax.html", linkTitle: "Markdown Syntax Documentation", linkText: "Syntax" },
    { linkUrl: "license.html", linkTitle: "Pricing and License Information", linkText: "License" },
]

viewSubmenu : Str -> Html.Node
viewSubmenu = \currentUrl ->
    ul
        [id "ProjectSubmenu"]
        (
            List.append
                (List.map navLinks \n -> viewNavLink currentUrl n)
                (
                    li [] [
                        a
                            [
                                href "https://daringfireball.net/projects/markdown/dingus",
                                title "Online Markdown Web Form",
                            ]
                            [text "Dingus"],
                    ]
                )
        )

viewNavLink : Str, NavLink -> Html.Node
viewNavLink = \currentUrl, { linkUrl, linkTitle, linkText } ->
    styleAttrs =
        if currentUrl == linkUrl then
            [class "selected"]
        else
            []
    attrs = List.concat styleAttrs [href linkUrl, title linkTitle]

    li [] [
        a attrs [text linkText],
    ]

viewPageHeading : Str -> Html.Node
viewPageHeading = \currentUrl ->
    headingText =
        List.findFirst navLinks (\n -> n.linkUrl == currentUrl)
        |> Result.map (\n -> n.linkUrl)
        |> Result.withDefault "Markdown"

    h1 [] [text headingText]
