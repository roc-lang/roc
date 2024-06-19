app [main] { pf: platform "https://github.com/lukewilliamboswell/basic-ssg/releases/download/0.1.0/EMH2OFwcXCUEzbwP6gyfeRQu7Phr-slc-vE8FPPreys.tar.br" }

import pf.Task exposing [Task]
import pf.SSG
import pf.Types exposing [Args]
import pf.Html exposing [header, nav, div, link, attribute, text, a, span, html, head, body, meta, script, footer, br]
import pf.Html.Attributes exposing [id, ariaLabel, ariaHidden, title, href, class, rel, type, content, lang, charset, name, color]
import InteractiveExample

main : Args -> Task {} _
main = \{ inputDir, outputDir } ->

    # get the path and url of markdown files in content directory
    files = SSG.files! inputDir

    # helper Task to process each file
    processFile = \{ path, relpath, url } ->

        inHtml = SSG.parseMarkdown! path

        outHtml = transform url inHtml

        SSG.writeFile { outputDir, relpath, content: outHtml }
    ## process each file
    Task.forEach! files processFile

pageData =
    Dict.empty {}
    |> Dict.insert "/abilities.html" { title: "Abilities | Roc", description: "Learn about abilities in the Roc programming language." }
    |> Dict.insert "/bdfn.html" { title: "Governance | Roc", description: "Learn about the governance model of the Roc programming language." }
    |> Dict.insert "/community.html" { title: "Community | Roc", description: "Connect with the community of the Roc programming language." }
    |> Dict.insert "/docs.html" { title: "Docs | Roc", description: "Documentation for the Roc programming language, including builtins." }
    |> Dict.insert "/donate.html" { title: "Donate | Roc", description: "Support the Roc programming language by donating or sponsoring." }
    |> Dict.insert "/faq.html" { title: "FAQ | Roc", description: "Frequently asked questions about the Roc programming language." }
    |> Dict.insert "/fast.html" { title: "Fast | Roc", description: "What does it mean that the Roc programming language is fast?" }
    |> Dict.insert "/friendly.html" { title: "Friendly | Roc", description: "What does it mean that the Roc programming language is friendly?" }
    |> Dict.insert "/functional.html" { title: "Functional | Roc", description: "What does it mean that the Roc programming language is functional?" }
    |> Dict.insert "/index.html" { title: "The Roc Programming Language", description: "A fast, friendly, functional language." }
    |> Dict.insert "/install/index.html" { title: "Install | Roc", description: "How to install the Roc programming language." }
    |> Dict.insert "/plans.html" { title: "Planned Changes | Roc", description: "Planned changes to the Roc programming language." }
    |> Dict.insert "/platforms.html" { title: "Platforms and Apps | Roc", description: "Learn about the platforms and applications architecture in the Roc programming language." }
    |> Dict.insert "/tutorial.html" { title: "Tutorial | Roc", description: "Learn the Roc programming language." }
    |> Dict.insert "/repl/index.html" { title: "REPL | Roc", description: "Try the Roc programming language in an online REPL." }
    |> Dict.insert "/examples/index.html" { title: "Examples | Roc", description: "All kinds of examples implemented in the Roc programming language." }
    |> Dict.insert "/install/other.html" { title: "Getting started on other systems | Roc", description: "Roc installation guide for other systems" }
    |> Dict.insert "/install/linux_x86_64.html" { title: "Getting started on Linux x86_64 | Roc", description: "Roc installation guide for Linux x86_64" }
    |> Dict.insert "/install/macos_apple_silicon.html" { title: "Getting started on MacOS Apple Silicon | Roc", description: "Roc installation guide for MacOS Apple Silicon" }
    |> Dict.insert "/install/macos_x86_64.html" { title: "Getting started on MacOS x86_64 | Roc", description: "Roc installation guide for MacOS x86_64" }
    |> Dict.insert "/install/windows.html" { title: "Getting started on Windows | Roc", description: "Roc installation guide for Windows" }
    |> Dict.insert "/install/nix.html" { title: "Getting started with Nix | Roc", description: "Roc installation guide for Nix" }
    |> Dict.insert "/install/getting_started.html" { title: "Getting started | Roc", description: "How to get started with Roc" }

getPageInfo : Str -> { title : Str, description : Str }
getPageInfo = \pagePathStr ->
    when Dict.get pageData pagePathStr is
        Ok pageInfo -> pageInfo
        Err KeyNotFound ->
            if Str.contains pagePathStr "/examples/" then
                Str.split pagePathStr "/"
                |> List.takeLast 2
                |> List.first # we use the folder for name for the page title, e.g. Json from examples/Json/README.html
                |> unwrapOrCrash "This List.first should never fail. pagePathStr ($(pagePathStr)) did not contain any `/`."
                |> (\pageTitle ->
                    { title: "$(pageTitle) | Roc", description: "$(pageTitle) example in the Roc programming language." })
            else
                crash "Web page $(pagePathStr) did not have a title and description specified in the pageData Dict. Please add one."

unwrapOrCrash : Result a b, Str -> a where b implements Inspect
unwrapOrCrash = \result, errorMsg ->
    when result is
        Ok val ->
            val

        Err err ->
            crash "$(Inspect.toStr err): $(errorMsg)"

transform : Str, Str -> Str
transform = \pagePathStr, htmlContent ->
    Html.render (view pagePathStr htmlContent)

preloadWoff2 : Str -> Html.Node
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
view = \pagePathStr, htmlContent ->
    mainBody =
        if pagePathStr == "/index.html" then
            when Str.splitFirst htmlContent "<!-- THIS COMMENT WILL BE REPLACED BY THE LARGER EXAMPLE -->" is
                Ok { before, after } -> [text before, InteractiveExample.view, text after]
                Err NotFound -> crash "Could not find the comment where the larger example on the homepage should have been inserted. Was it removed or edited?"
        else
            [text htmlContent]

    bodyAttrs =
        when pagePathStr is
            "/index.html" -> [id "homepage-main"]
            "/tutorial.html" -> [id "tutorial-main", class "article-layout"]
            _ ->
                if Str.startsWith pagePathStr "/examples/" && pagePathStr != "/examples/index.html" then
                    # Individual examples should render wider than articles.
                    # Otherwise the width is unreasonably low for the code blocks,
                    # and those pages don't tend to have big paragraphs anyway.
                    # Keep the article width on examples/index.html though,
                    # because otherwise when you're clicking through the top nav links,
                    # /examples has a surprisingly different width from the other links.
                    [id "example-main"]
                else
                    [class "article-layout"]

    pageInfo = getPageInfo pagePathStr

    html [lang "en", class "no-js"] [
        head [] [
            meta [charset "utf-8"],
            Html.title [] [text (pageInfo.title)],
            meta [name "description", content (pageInfo.description)],
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
            viewNavbar pagePathStr,
            Html.main [] mainBody,
            footer [] [
                div [id "footer"] [
                    div [id "gh-link"] [
                        a [id "gh-centered-link", href "https://github.com/roc-lang/roc"] [
                            ghLogo,
                            span [id "gh-link-text"] [text "roc-lang/roc"],
                        ],
                    ],
                    br [] [],
                    text " powered by ",
                    a [href "https://www.netlify.com"] [text "Netlify"],
                ],
            ],
        ],
    ]

viewNavbar : Str -> Html.Node
viewNavbar = \pagePathStr ->
    isHomepage = pagePathStr == "/index.html"

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

ghLogo : Html.Node
ghLogo =
    (Html.element "svg")
        [
            (Html.attribute "viewBox") "0 0 98 96",
            (Html.attribute "height") "25",
            (Html.attribute "xmlns") "http://www.w3.org/2000/svg",
            (Html.attribute "fill-rule") "evenodd",
            (Html.attribute "clip-rule") "evenodd",
            (Html.attribute "role") "img",
            id "gh-logo",
        ]
        [
            (Html.element "path") [(Html.attribute "d") "M48.854 0C21.839 0 0 22 0 49.217c0 21.756 13.993 40.172 33.405 46.69 2.427.49 3.316-1.059 3.316-2.362 0-1.141-.08-5.052-.08-9.127-13.59 2.934-16.42-5.867-16.42-5.867-2.184-5.704-5.42-7.17-5.42-7.17-4.448-3.015.324-3.015.324-3.015 4.934.326 7.523 5.052 7.523 5.052 4.367 7.496 11.404 5.378 14.235 4.074.404-3.178 1.699-5.378 3.074-6.6-10.839-1.141-22.243-5.378-22.243-24.283 0-5.378 1.94-9.778 5.014-13.2-.485-1.222-2.184-6.275.486-13.038 0 0 4.125-1.304 13.426 5.052a46.97 46.97 0 0 1 12.214-1.63c4.125 0 8.33.571 12.213 1.63 9.302-6.356 13.427-5.052 13.427-5.052 2.67 6.763.97 11.816.485 13.038 3.155 3.422 5.015 7.822 5.015 13.2 0 18.905-11.404 23.06-22.324 24.283 1.78 1.548 3.316 4.481 3.316 9.126 0 6.6-.08 11.897-.08 13.526 0 1.304.89 2.853 3.316 2.364 19.412-6.52 33.405-24.935 33.405-46.691C97.707 22 75.788 0 48.854 0z"] [],
        ]
