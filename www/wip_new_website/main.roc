app "roc-website"
    packages { pf: "../../examples/static-site-gen/platform/main.roc" }
    imports [
        pf.Html.{ Node, html, head, body, header, footer, div, main, text, nav, a, link, meta, script },
        pf.Html.Attributes.{ attribute, content, name, id, href, rel, lang, class, title, charset, color, ariaLabel, type },
        InteractiveExample,
    ]
    provides [transformFileContent] to pf

pageData =
    Dict.empty {}
    |> Dict.insert "community.html" { title: "Community", description: "The Roc community" }
    |> Dict.insert "design_goals.html" { title: "Design Goals", description: "Roc's design goals" }
    |> Dict.insert "docs.html" { title: "Documentation", description: "Learn the Roc programming language" }
    |> Dict.insert "index.html" { title: "Roc", description: "The Roc programming language" }
    |> Dict.insert "install.html" { title: "Install", description: "Getting started with the Roc programming language" }
    |> Dict.insert "donate.html" { title: "Donate", description: "Sponsor Roc" }
    |> Dict.insert "tutorial.html" { title: "Tutorial", description: "The Roc tutorial" }

getPage : Str -> { title : Str, description : Str }
getPage = \current ->
    Dict.get pageData current
    |> Result.withDefault { title: "", description: "" }

getTitle : Str -> Str
getTitle = \current ->
    getPage current |> .title

getDescription : Str -> Str
getDescription = \current ->
    getPage current |> .description

transformFileContent : Str, Str -> Str
transformFileContent = \page, htmlContent ->
    Html.render (view page htmlContent)

preloadWoff2 : Str -> Node
preloadWoff2 = \url ->
    link [rel "preload", (attribute "as") "font", type "font/woff2", href url]

view : Str, Str -> Html.Node
view = \page, htmlContent ->
    mainBody =
        if page == "index.html" then
            [text htmlContent, InteractiveExample.view]
        else
            [text htmlContent]

    html [lang "en", class "no-js"] [
        head [] [
            meta [charset "utf-8"],
            Html.title [] [text (getTitle page)],
            meta [name "description", content (getDescription page)],
            meta [name "viewport", content "width=device-width"],
            link [rel "stylesheet", href "/wip/site.css"],
            link [rel "stylesheet", href "/wip/repl.css"],
            preloadWoff2 "/fonts/merriweather-v30-latin/merriweather-v30-latin-regular.woff2",
            preloadWoff2 "/fonts/merriweather-v30-latin-ext_latin/merriweather-v30-latin-ext_latin-regular.woff2",
            preloadWoff2 "/fonts/lato-v23-latin-ext_latin/lato-v23-latin-ext_latin-regular.woff2",
            preloadWoff2 "/fonts/lato-v23-latin/lato-v23-latin-regular.woff2",
            preloadWoff2 "/fonts/source-code-pro-v22-latin-ext_latin/source-code-pro-v22-latin-ext_latin-regular.woff2",
            preloadWoff2 "/fonts/source-code-pro-v22-latin/source-code-pro-v22-latin-regular.woff2",
            link [rel "prefetch", href "/repl/roc_repl_wasm.js"],
            link [rel "icon", href "/favicon.svg"],
            # Safari ignores rel="icon" and only respects rel="mask-icon". It will render the SVG with
            # fill="#000" unless this `color` attribute here is hardcoded (not a CSS `var()`) to override it.
            link [rel "mask-icon", href "/favicon.svg", color "#7d59dd"],
            # Remove the .no-js class from <html> before the body renders, so anything
            # hidden via CSS using a .no-js selector will apply to the initial layout
            # of the body instead of having a flash of content that immediately gets hidden.
            #
            # WARNING: Updating this requires updating its sha256 in netlify.toml under Content-Security-Policy.
            #          Otherwise, this will work locally and then fail in production!
            script [] [text "document.documentElement.className = document.documentElement.className.replace('no-js', '');"]
        ],
        body [] [
            viewNavbar page,
            main [] mainBody,
            footer [] [
                div [id "footer"] [
                    text " powered by ",
                    a [href "https://www.netlify.com"] [text "Netlify"],
                ],
            ],
        ],
    ]

viewNavbar : Str -> Html.Node
viewNavbar = \page ->
    logo = if page == "index.html" then [] else [rocLogo]

    header [id "top-bar"] [
        nav [ariaLabel "primary"] [
            a [id "nav-home-link", href "/wip/index.html", title "The Roc Programming Language"] logo,
            div [id "top-bar-links"] [
                a [href "/wip/tutorial.html"] [text "tutorial"],
                a [href "/wip/install.html"] [text "install"],
                a [href "/wip/community.html"] [text "community"],
                a [href "/wip/docs.html"] [text "docs"],
                a [href "/wip/donate.html"] [text "donate"],
            ],
        ],
    ]

rocLogo : Html.Node
rocLogo =
    (Html.element "svg")
        [
            (Html.attribute "viewBox") "0 -6 51 58",
            (Html.attribute "xmlns") "http://www.w3.org/2000/svg",
            (Html.attribute "aria-labelledby") "logo-link",
            (Html.attribute "role") "img",
            class "roc-logo",
        ]
        [
            (Html.element "title") [id "logo-link"] [text "Return to Roc Home"],
            (Html.element "polygon")
                [
                    (Html.attribute "role") "presentation",
                    (Html.attribute "points") "0,0 23.8834,3.21052 37.2438,19.0101 45.9665,16.6324 50.5,22 45,22 44.0315,26.3689 26.4673,39.3424 27.4527,45.2132 17.655,53 23.6751,22.7086",
                ]
                [],
        ]
