interface Html.Internal
    exposes [
        Html,
        initServerApp,
        initClientApp,
    ]
    imports []

Action state : [Action state]

Html state : [
    None,
    Element (state -> Action state) (Html state),
]

RenderedHtml : [
    RenderedNone,
]

translateStatic : Html state -> Html *

initServerApp : {} -> Html []
initServerApp = \{} ->
    translateStatic (Element Action None)

initClientApp : {} -> { thing : RenderedHtml }
initClientApp = \{} ->
    indexNodes { thing: RenderedNone } (Element Action None)

indexNodes : { thing : RenderedHtml }, Html state -> { thing : RenderedHtml }
indexNodes = \{ thing }, unrendered ->
    List.walk [] { thing } indexNodes
