expect
    html : Html {}
    html =
        Element "a" 43 [HtmlAttr "href" "https://www.roc-lang.org/"] [Text "Roc"]

    actual : { nodes : List RenderedNode, siblingIds : List U64 }
    actual =
        indexNodes { nodes: [], siblingIds: [] } html

    expected : { nodes : List RenderedNode, siblingIds : List U64 }
    expected = {
        nodes: [
            RenderedText "Roc",
            RenderedElement "a" { emptyRenderedAttrs & htmlAttrs: Dict.fromList [("href", "https://www.roc-lang.org/")] } [0],
        ],
        siblingIds: [1],
    }

    (actual.nodes == expected.nodes)
    and (actual.siblingIds == expected.siblingIds)
