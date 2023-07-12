app "roc-website"
    packages { pf: "../../examples/static-site-gen/platform/main.roc" }    
    imports [
        pf.Html.{ html, head, body, footer, p, div, main, text, nav, a, link, meta },
        pf.Html.Attributes.{ content, name, id, href, rel, lang, class, title, charset },
    ]
    provides [transformFileContent] to pf

pageData =  
    Dict.empty {} 
    |> Dict.insert "community.html" { title: "Community", description: "The Roc community" }
    |> Dict.insert "design_goals.html" { title: "Design Goals", description: "Roc's design goals" }
    |> Dict.insert "docs.html" { title: "Documentation", description: "Learn the Roc programming language" }
    |> Dict.insert "home.html" { title: "Roc Lang", description: "The Roc programming language" }
    |> Dict.insert "install.html" { title: "Install", description: "Getting started with the Roc programming language" }
    |> Dict.insert "sponsor.html" { title: "Sponsor", description: "Sponsor Roc" }
    |> Dict.insert "tutorial.html" { title: "Tutorial", description: "The Roc tutorial" }

getPage : Str -> {title : Str, description : Str}
getPage = \current ->
    Dict.get pageData current
    |> Result.withDefault { title: "", description: ""}

getTitle : Str -> Str
getTitle = \current -> 
    getPage current |> .title

getDescription : Str -> Str
getDescription = \current -> 
    getPage current |> .description

transformFileContent : Str, Str -> Str
transformFileContent = \page, htmlContent ->
    Html.render (view page htmlContent)

view : Str, Str -> Html.Node
view = \page, htmlContent ->
    html [lang "en"] [
        head [] [
            meta [charset "utf-8"] [],
            Html.title [] [text (getTitle page)],
            meta [name "description", content (getDescription page)] [],
            meta [name "viewport", content "width=device-width"] [],
            link [rel "stylesheet", href "/wip/site.css"] [],
            link [rel "icon", href "/favicon.svg"] [],
        ],
        body [] [
            viewNavbar,
            main [] [
                text htmlContent,
            ],
            footer [
                id "footer"
            ] [
                p [] [
                    a [href "https://github.com/roc-lang/roc"] [text "source code repository"],
                ],
                # <!-- TODO FOOTER - Lanugage link to source code -->
                text "This site is powered by ",
                a [href "https://www.netlify.com"] [ text "Netlify"],
                text ". Made by people who like to make nice things. © Roc 2023",
            ]
        ],
        # TODO - add site.js if needed
        # script [src "/site.js"] [],
    ]

viewNavbar : Html.Node
viewNavbar =
    div [id "top-bar"] [
        nav [] [
            a [href "/wip/index.html", title "The Roc Programming Language"] [
                rocLogo
            ],
            div [id "top-bar-links"] [
                a [href "/wip/tutorial.html"] [text "tutorial"],
                a [href "/wip/install.html"] [text "install"],
                a [href "#todo-link-to-examples-site"] [text "examples"],
                a [href "/wip/community.html"] [text "community"],
                a [href "/wip/sponsor.html"] [text "sponsor"],
                a [href "/wip//docs.html"] [text "docs"],
            ],
        ],
    ]

rocLogo = 
    (Html.element "svg") [
            (Html.attribute "viewBox") "0 -6 51 58",
            (Html.attribute "fill") "#7c38f5",
            (Html.attribute "xmlns") "http://www.w3.org/2000/svg",
            (Html.attribute "aria-labelledby") "logo-link",
            (Html.attribute "role") "img",
            class "roc-logo"
        ] [
            (Html.element "title") [id "logo-link"] [text "Return to Roc Home"],
            (Html.element "polygon") [
                    (Html.attribute "role") "presentation",
                    (Html.attribute "points") "0,0 23.8834,3.21052 37.2438,19.0101 45.9665,16.6324 50.5,22 45,22 44.0315,26.3689 26.4673,39.3424 27.4527,45.2132 17.655,53 23.6751,22.7086",
                ] [],
        ]
