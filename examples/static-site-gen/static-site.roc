app "static-site"
    packages { pf: "platform/main.roc" }
    imports [
        pf.Html.{ html, head, body, div, text, a, ul, li, link, meta },
        pf.Html.Attributes.{ httpEquiv, content, href, rel, lang, class, title },
    ]
    provides [transformFileContent] to pf

NavLink : {
    url : Str,
    title : Str,
    text : Str,
}

navLinks : List NavLink
navLinks = [
    { url: "apple.html", title: "Exempli Gratia Pagina Pomi", text: "Apple" },
    { url: "banana.html", title: "Exempli Gratia Pagina Musa", text: "Banana" },
    { url: "cherry.html", title: "Exempli Pagina Cerasus", text: "Cherry" },
]

transformFileContent : Str, Str -> Str
transformFileContent = \currentUrl, htmlContent ->
    List.findFirst navLinks (\{ url } -> url == currentUrl)
    |> Result.map (\currentNavLink -> view currentNavLink htmlContent)
    |> Result.map Html.render
    |> Result.withDefault ""

view : NavLink, Str -> Html.Node
view = \currentNavLink, htmlContent ->
    html [lang "en"] [
        head [] [
            meta [httpEquiv "content-type", content "text/html; charset=utf-8"] [],
            Html.title [] [text currentNavLink.title],
            link [rel "stylesheet", href "style.css"] [],
        ],
        body [] [
            div [class "main"] [
                div [class "navbar"] [
                    viewNavbar currentNavLink,
                ],
                div [class "article"] [
                    # For now `text` is not escaped so we can use it to insert HTML
                    # We'll probably want something more explicit in the long term though!
                    text htmlContent,
                ],
            ],
        ],
    ]

viewNavbar : NavLink -> Html.Node
viewNavbar = \currentNavLink ->
    ul
        []
        (List.map navLinks \nl -> viewNavLink (nl == currentNavLink) nl)

viewNavLink : Bool, NavLink -> Html.Node
viewNavLink = \isCurrent, navlink ->
    if isCurrent then
        li [class "nav-link nav-link--current"] [
            text navlink.text,
        ]
    else
        li [class "nav-link"] [
            a
                [href navlink.url, title navlink.title]
                [text navlink.text],
        ]
