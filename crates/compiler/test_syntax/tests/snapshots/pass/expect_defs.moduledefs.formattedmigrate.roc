expect
    html : Html {}
    html =
        Element "a" 43 [HtmlAttr "href" "https://www.roc-lang.org/"] [Text "Roc"]

    actual : { nodes : List RenderedNode, sibling_ids : List U64 }
    actual =
        index_nodes { nodes: [], sibling_ids: [] } html

    expected : { nodes : List RenderedNode, sibling_ids : List U64 }
    expected = {
        nodes: [
            RenderedText "Roc",
            RenderedElement "a" { empty_rendered_attrs & html_attrs: Dict.from_list [("href", "https://www.roc-lang.org/")] } [0],
        ],
        sibling_ids: [1],
    }

    (actual.nodes == expected.nodes)
    && (actual.sibling_ids == expected.sibling_ids)
