app [main!] {
    pf: platform "https://github.com/lukewilliamboswell/basic-ssg/releases/download/0.7.0/lVVvjgCyBy6QmqXX0CmK96l67gqxiF5JeszPeAj2NKU.tar.gz",
}

import pf.SSG
import pf.Types
import pf.Html exposing [header, nav, div, link, attribute, text, a, span, html, head, body, meta, script, footer, br]
import pf.Html.Attributes exposing [id, aria_label, aria_hidden, title, href, class, rel, type, content, lang, charset, name, color]
import "content/tutorial.md" as tutorial_markdown : Str

import InteractiveExample

main! = \{ input_dir, output_dir } ->

    try(SSG.write_file!({ output_dir, relpath: Types.to_rel_path("llms.txt"), content: tutorial_markdown }))

    # get the path and url of markdown files in content directory
    files = try(SSG.files!(input_dir))

    process_file! = \{ path, relpath, url } ->

        in_html = try(SSG.parse_markdown!(path))

        out_html = transform(url, in_html)

        SSG.write_file!({ output_dir, relpath, content: out_html })

    ## process each file
    List.for_each_try!(files, process_file!)

page_data =
    Dict.empty({})
    |> Dict.insert("/abilities.html", { title: "Abilities | Roc", description: "Learn about abilities in the Roc programming language." })
    |> Dict.insert("/bdfn.html", { title: "Governance | Roc", description: "Learn about the governance model of the Roc programming language." })
    |> Dict.insert("/community.html", { title: "Community | Roc", description: "Connect with the community of the Roc programming language." })
    |> Dict.insert("/docs.html", { title: "Docs | Roc", description: "Documentation for the Roc programming language, including builtins." })
    |> Dict.insert("/donate.html", { title: "Donate | Roc", description: "Support the Roc programming language by donating or sponsoring." })
    |> Dict.insert("/faq.html", { title: "FAQ | Roc", description: "Frequently asked questions about the Roc programming language." })
    |> Dict.insert("/fast.html", { title: "Fast | Roc", description: "What does it mean that the Roc programming language is fast?" })
    |> Dict.insert("/friendly.html", { title: "Friendly | Roc", description: "What does it mean that the Roc programming language is friendly?" })
    |> Dict.insert("/functional.html", { title: "Functional | Roc", description: "What does it mean that the Roc programming language is functional?" })
    |> Dict.insert("/index.html", { title: "The Roc Programming Language", description: "A fast, friendly, functional language." })
    |> Dict.insert("/install/index.html", { title: "Install | Roc", description: "How to install the Roc programming language." })
    |> Dict.insert("/plans.html", { title: "Planned Changes | Roc", description: "Planned changes to the Roc programming language." })
    |> Dict.insert("/platforms.html", { title: "Platforms and Apps | Roc", description: "Learn about the platforms and applications architecture in the Roc programming language." })
    |> Dict.insert("/tutorial.html", { title: "Tutorial | Roc", description: "Learn the Roc programming language." })
    |> Dict.insert("/different-names.html", { title: "Different Names | Roc", description: "Names of things in Roc that differ from other languages." })
    |> Dict.insert("/repl/index.html", { title: "REPL | Roc", description: "Try the Roc programming language in an online REPL." })
    |> Dict.insert("/examples/index.html", { title: "Examples | Roc", description: "All kinds of examples implemented in the Roc programming language." })
    |> Dict.insert("/install/other.html", { title: "Getting started on other systems | Roc", description: "Roc installation guide for other systems" })
    |> Dict.insert("/install/linux_x86_64.html", { title: "Getting started on Linux x86_64 | Roc", description: "Roc installation guide for Linux x86_64" })
    |> Dict.insert("/install/macos_apple_silicon.html", { title: "Getting started on MacOS Apple Silicon | Roc", description: "Roc installation guide for MacOS Apple Silicon" })
    |> Dict.insert("/install/macos_x86_64.html", { title: "Getting started on MacOS x86_64 | Roc", description: "Roc installation guide for MacOS x86_64" })
    |> Dict.insert("/install/windows.html", { title: "Getting started on Windows | Roc", description: "Roc installation guide for Windows" })
    |> Dict.insert("/install/nix.html", { title: "Getting started with Nix | Roc", description: "Roc installation guide for Nix" })
    |> Dict.insert("/install/getting_started.html", { title: "Getting started | Roc", description: "How to get started with Roc" })

get_page_info : Str -> { title : Str, description : Str }
get_page_info = \page_path_str ->
    when Dict.get(page_data, page_path_str) is
        Ok(page_info) -> page_info
        Err(KeyNotFound) ->
            if Str.contains(page_path_str, "/examples/") then
                Str.split_on(page_path_str, "/")
                |> List.take_last(2)
                |> List.first # we use the folder for name for the page title, e.g. Json from examples/Json/README.html
                |> unwrap_or_crash("This List.first should never fail. page_path_str (${page_path_str}) did not contain any `/`.")
                |> (\page_title ->
                    { title: "${page_title} | Roc", description: "${page_title} example in the Roc programming language." })
            else
                crash("Web page ${page_path_str} did not have a title and description specified in the pageData Dict. Please add one.")

unwrap_or_crash : Result a b, Str -> a where b implements Inspect
unwrap_or_crash = \result, error_msg ->
    when result is
        Ok(val) ->
            val

        Err(err) ->
            crash("${Inspect.to_str(err)}: ${error_msg}")

transform : Str, Str -> Str
transform = \page_path_str, html_content ->
    Html.render(view(page_path_str, html_content))

preload_woff2 : Str -> Html.Node
preload_woff2 = \url ->

    link([
        rel("preload"),
        attribute("as")("font"),
        type("font/woff2"),
        href(url),
        # Necessary for preloading fonts, even if the request won't be cross-origin
        # https://stackoverflow.com/a/70878420
        attribute("crossorigin")("anonymous"),
    ])

view : Str, Str -> Html.Node
view = \page_path_str, html_content ->
    main_body =
        if page_path_str == "/index.html" then
            when Str.split_first(html_content, "<!-- THIS COMMENT WILL BE REPLACED BY THE LARGER EXAMPLE -->") is
                Ok({ before, after }) -> [text(before), InteractiveExample.view, text(after)]
                Err(NotFound) -> crash("Could not find the comment where the larger example on the homepage should have been inserted. Was it removed or edited?")
        else
            [text(html_content)]

    body_attrs =
        when page_path_str is
            "/index.html" -> [id("homepage-main")]
            "/tutorial.html" -> [id("tutorial-main"), class("article-layout")]
            _ ->
                if Str.starts_with(page_path_str, "/examples/") and page_path_str != "/examples/index.html" then
                    # Individual examples should render wider than articles.
                    # Otherwise the width is unreasonably low for the code blocks,
                    # and those pages don't tend to have big paragraphs anyway.
                    # Keep the article width on examples/index.html though,
                    # because otherwise when you're clicking through the top nav links,
                    # /examples has a surprisingly different width from the other links.
                    [id("example-main")]
                else
                    [class("article-layout")]

    page_info = get_page_info(page_path_str)

    html([lang("en"), class("no-js")], [
        head([], [
            meta([charset("utf-8")]),
            Html.title([], [text(page_info.title)]),
            meta([name("description"), content(page_info.description)]),
            meta([name("viewport"), content("width=device-width")]),
            link([rel("icon"), href("/favicon.svg")]),
            # Preload the latin-regular (but not latin-ext) unicode ranges of our fonts.
            # The homepage doesn't actually use latin-ext
            preload_woff2("/fonts/lato-v23-latin/lato-v23-latin-regular.woff2"),
            preload_woff2("/fonts/source-code-pro-v22-latin/source-code-pro-v22-latin-regular.woff2"),
            preload_woff2("/fonts/permanent-marker-v16-latin/permanent-marker-v16-latin-regular.woff2"),
            link([rel("prefetch"), href("/repl/roc_repl_wasm.js")]),
            link([rel("stylesheet"), href("/site.css")]),
            # Safari ignores rel="icon" and only respects rel="mask-icon". It will render the SVG with
            # fill="#000" unless this `color` attribute here is hardcoded (not a CSS `var()`) to override it.
            link([rel("mask-icon"), href("/favicon.svg"), color("#7d59dd")]),
            # Remove the .no-js class from <html> before the body renders, so anything
            # hidden via CSS using a .no-js selector will apply to the initial layout
            # of the body instead of having a flash of content that immediately gets hidden.
            #
            # WARNING: Updating this requires updating its sha256 in netlify.toml under Content-Security-Policy.
            #          Otherwise, this will work locally and then fail in production!
            script([], [text("document.documentElement.className = document.documentElement.className.replace('no-js', '');")]),
        ]),
        body(body_attrs, [
            view_navbar(page_path_str),
            Html.main([], main_body),
            footer([], [
                div([id("footer")], [
                    div([id("gh-link")], [
                        a([id("gh-centered-link"), href("https://github.com/roc-lang/roc")], [
                            gh_logo,
                            span([id("gh-link-text")], [text("roc-lang/roc")]),
                        ]),
                    ]),
                    br([], []),
                    text(" powered by "),
                    a([href("https://www.netlify.com")], [text("Netlify")]),
                ]),
            ]),
        ]),
    ])

view_navbar : Str -> Html.Node
view_navbar = \page_path_str ->
    is_homepage = page_path_str == "/index.html"

    home_link_attrs =
        [id("nav-home-link"), href("/"), title("The Roc Programming Language Homepage")]
        |> List.concat((if is_homepage then [aria_hidden("true")] else []))

    header([id("top-bar")], [
        nav([aria_label("primary")], [
            a(home_link_attrs, [roc_logo, span([class("home-link-text")], [text("Roc")])]),
            div([id("top-bar-links")], [
                a([href("/tutorial")], [text("tutorial")]),
                a([href("/install")], [text("install")]),
                a([href("/examples")], [text("examples")]),
                a([href("/community")], [text("community")]),
                a([href("/docs")], [text("docs")]),
                a([href("/donate")], [text("donate")]),
            ]),
        ]),
    ])

roc_logo : Html.Node
roc_logo =
    Html.element("svg")(
        [
            Html.attribute("viewBox")("0 -6 51 58"),
            Html.attribute("xmlns")("http://www.w3.org/2000/svg"),
            Html.attribute("aria-labelledby")("logo-link"),
            Html.attribute("role")("img"),
            class("roc-logo"),
        ],
        [
            Html.element("title")([id("logo-link")], [text("Return to Roc Home")]),
            Html.element("polygon")(
                [
                    Html.attribute("role")("presentation"),
                    Html.attribute("points")("0,0 23.8834,3.21052 37.2438,19.0101 45.9665,16.6324 50.5,22 45,22 44.0315,26.3689 26.4673,39.3424 27.4527,45.2132 17.655,53 23.6751,22.7086"),
                ],
                [],
            ),
        ],
    )

gh_logo : Html.Node
gh_logo =
    Html.element("svg")(
        [
            Html.attribute("viewBox")("0 0 98 96"),
            Html.attribute("height")("25"),
            Html.attribute("xmlns")("http://www.w3.org/2000/svg"),
            Html.attribute("fill-rule")("evenodd"),
            Html.attribute("clip-rule")("evenodd"),
            Html.attribute("role")("img"),
            id("gh-logo"),
        ],
        [
            Html.element("path")([Html.attribute("d")("M48.854 0C21.839 0 0 22 0 49.217c0 21.756 13.993 40.172 33.405 46.69 2.427.49 3.316-1.059 3.316-2.362 0-1.141-.08-5.052-.08-9.127-13.59 2.934-16.42-5.867-16.42-5.867-2.184-5.704-5.42-7.17-5.42-7.17-4.448-3.015.324-3.015.324-3.015 4.934.326 7.523 5.052 7.523 5.052 4.367 7.496 11.404 5.378 14.235 4.074.404-3.178 1.699-5.378 3.074-6.6-10.839-1.141-22.243-5.378-22.243-24.283 0-5.378 1.94-9.778 5.014-13.2-.485-1.222-2.184-6.275.486-13.038 0 0 4.125-1.304 13.426 5.052a46.97 46.97 0 0 1 12.214-1.63c4.125 0 8.33.571 12.213 1.63 9.302-6.356 13.427-5.052 13.427-5.052 2.67 6.763.97 11.816.485 13.038 3.155 3.422 5.015 7.822 5.015 13.2 0 18.905-11.404 23.06-22.324 24.283 1.78 1.548 3.316 4.481 3.316 9.126 0 6.6-.08 11.897-.08 13.526 0 1.304.89 2.853 3.316 2.364 19.412-6.52 33.405-24.935 33.405-46.691C97.707 22 75.788 0 48.854 0z")], []),
        ],
    )
