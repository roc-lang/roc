interface Html.Internal
    exposes [
        Html,
        initServerApp,
        initClientApp,
    ]
    imports []

Action state : [Action state]

Html state : [
    Element (state -> Action state) (Html state),
]

RenderedHtml : [
    RenderedNone,
]

translateStatic : Html state -> Html *

initServerApp : {} -> Html []
initServerApp = \{} ->
    translateStatic (Element [] [])

initClientApp : {} -> { thing : RenderedHtml }
initClientApp = \{} ->
    indexNodes { thing: RenderedNone } (Element [] [])

indexNodes : { thing : RenderedHtml }, Html state -> { thing : RenderedHtml }
indexNodes = \{ thing }, unrendered ->
    List.walk [] { thing } indexNodes
