app "rocLovesPlatforms"
    packages { pf: "c-platform/main.roc" }
    # To switch platforms, comment-out the line above and un-comment one below.
    # packages { pf: "rust-platform/main.roc" }
    # packages { pf: "swift-platform/main.roc" }
    # packages { pf: "web-assembly-platform/main.roc" } # See ./web-assembly-platform/README.md
    # packages { pf: "zig-platform/main.roc" }
    imports []
    provides [main] to pf

Html state : [
    Element (List (Html state)),
]

translateStatic : Html _ -> Html _
translateStatic = \node ->
    when node is
        Element children ->
            newChildren = List.map children translateStatic

            Element newChildren

main = when translateStatic (Element []) is
    _ -> ""
