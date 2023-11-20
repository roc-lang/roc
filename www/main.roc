app "roc-website"
    packages { pf: "../examples/static-site-gen/platform/main.roc" }
    imports [
        pf.Html.{ Node, html, head, body, header, footer, div, span, main, text, nav, a, link, meta, script },
        pf.Html.Attributes.{ attribute, content, name, id, href, rel, lang, class, title, charset, color, ariaLabel, ariaHidden, type },
        InteractiveExample,
    ]
    provides [transformFileContent] to pf

pageData =
    Dict.empty {}
    |> Dict.insert "community.html" { title: "Roc Community", description: "Connect with the Roc programming language community" }
    |> Dict.insert "docs.html" { title: "Roc Docs", description: "Documentation for the Roc programming language, including builtins" }
    |> Dict.insert "index.html" { title: "The Roc Programming Language", description: "A fast, friendly, functional language" }
    |> Dict.insert "install.html" { title: "Install Roc", description: "Install the Roc programming language" }
    |> Dict.insert "donate.html" { title: "Donate to Roc", description: "Support the Roc programming language by donating or sponsoring" }
    |> Dict.insert "tutorial.html" { title: "Roc Tutorial", description: "Learn the Roc programming language" }

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
    link [
        rel "preload",
        (attribute "as") "font",
        type "font/woff2",
        href url,
        # Necessary for preloading fonts, even if the request won't be cross-origin
        # https://stackoverflow.com/a/70878420
        (attribute "crossorigin") "anonymous",
    ]

view : Str, Str -> Html.Node
view = \page, htmlContent ->
    mainBody =
        if page == "index.html" then
            when Str.splitFirst htmlContent "<!-- THIS COMMENT WILL BE REPLACED BY THE LARGER EXAMPLE -->" is
                Ok { before, after } -> [text before, InteractiveExample.view, text after]
                Err NotFound -> crash "Could not find the comment where the larger example on the homepage should have been inserted. Was it removed or edited?"
        else
            [text htmlContent]

    bodyAttrs =
        when page is
            "index.html" -> [id "homepage-main"]
            "tutorial.html" -> [id "tutorial-main", class "article-layout"]
            _ ->
                if Str.startsWith page "examples/" && page != "examples/index.html" then
                    # Individual examples should render wider than articles.
                    # Otherwise the width is unreasonably low for the code blocks,
                    # and those pages don't tend to have big paragraphs anyway.
                    # Keep the article width on examples/index.html though,
                    # because otherwise when you're clicking through the top nav links,
                    # /examples has a surprisingly different width from the other links.
                    [id "example-main"]
                else
                    [class "article-layout"]

    html [lang "en", class "no-js"] [
        head [] [
            meta [charset "utf-8"],
            Html.title [] [text (getTitle page)],
            meta [name "description", content (getDescription page)],
            meta [name "viewport", content "width=device-width"],
            link [rel "icon", href "/favicon.svg"],
            # Preload the latin-regular (but not latin-ext) unicode ranges of our fonts.
            # The homepage doesn't actually use latin-ext
            preloadWoff2 "/fonts/lato-v23-latin/lato-v23-latin-regular.woff2",
            preloadWoff2 "/fonts/source-code-pro-v22-latin/source-code-pro-v22-latin-regular.woff2",
            preloadWoff2 "/fonts/permanent-marker-v16-latin/permanent-marker-v16-latin-regular.woff2",
            link [rel "prefetch", href "/repl/roc_repl_wasm.js"],
            link [rel "stylesheet", href "/site.css"],
            # Safari ignores rel="icon" and only respects rel="mask-icon". It will render the SVG with
            # fill="#000" unless this `color` attribute here is hardcoded (not a CSS `var()`) to override it.
            link [rel "mask-icon", href "/favicon.svg", color "#7d59dd"],
            # Remove the .no-js class from <html> before the body renders, so anything
            # hidden via CSS using a .no-js selector will apply to the initial layout
            # of the body instead of having a flash of content that immediately gets hidden.
            #
            # WARNING: Updating this requires updating its sha256 in netlify.toml under Content-Security-Policy.
            #          Otherwise, this will work locally and then fail in production!
            script [] [text "document.documentElement.className = document.documentElement.className.replace('no-js', '');"],
        ],
        body bodyAttrs [
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
    isHomepage = page == "index.html"

    homeLinkAttrs =
        [id "nav-home-link", href "/", title "The Roc Programming Language Homepage"]
        |> List.concat (if isHomepage then [ariaHidden "true"] else [])

    header [id "top-bar"] [
        nav [ariaLabel "primary"] [
            a homeLinkAttrs [rocLogo, span [class "home-link-text"] [text "Roc"]],
            div [id "top-bar-links"] [
                a [href "/tutorial"] [text "tutorial"],
                a [href "/install"] [text "install"],
                a [href "/examples"] [text "examples"],
                a [href "/community"] [text "community"],
                a [href "/docs"] [text "docs"],
                a [href "/donate"] [text "donate"],
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
