interface Html.Internal
    exposes [
        App,
        Html,
        Handler,
        initServerApp,
        initClientApp,
    ]
    imports [
        Action.{ Action },
    ]

App state initData : {
    init : initData -> state,
}

Html state : [
    Element (Handler state) (Html state),
]

RenderedHtml : [
    RenderedNone,
]

Handler state := [
    Normal (state -> Action state),
]

translateStatic : Html state -> Html *

initServerApp : App state initData, initData -> Html []
initServerApp = \app, initData ->
    translateStatic (Element [] [])

initClientApp : List U8, App state initData -> { list : List RenderedHtml, index : Nat }
initClientApp = \json, app ->
    indexNodes { list: [], index: 0 } (Element [] [])

indexNodes : { list : List RenderedHtml, index : Nat }, Html state -> { list : List RenderedHtml, index : Nat }
indexNodes = \{ list, index }, unrendered ->
    List.walk [] { list, index } indexNodes
