interface Html.Internal
    exposes [
        App,
        Html,
        Attribute,
        CyclicStructureAccessor,
        Handler,
        element,
        text,
        # lazy, TODO
        none,
        translate,
        translateStatic,
        initServerApp,
        initClientApp,
        insertHandler,
        replaceHandler,
        dispatchEvent,
        appendRenderedStatic,
        nodeSize,
    ]
    imports [
        Action.{ Action },
        Effect.{ Effect },
        Encode,
        Json,
        Html.HostJavaScript.{ hostJavaScript },
    ]

PlatformState state initData : {
    app : App state initData,
    state,
    views : Dict HtmlId (Html state),
    handlers : HandlerLookup state,
}

# TODO: keep a list of free indices that we push and pop like a stack
HandlerLookup state : List (Result (Handler state) [NoHandler])

App state initData : {
    static : Html [],
    initDynamic : initData -> state,
    renderDynamic : state -> Dict HtmlId (Html state),
    wasmUrl : Str,
}

HtmlId : Str

Html state : [
    None,
    Text JsIndex Str,
    Element Str JsIndex Nat (List (Attribute state)) (List (Html state)),
    Lazy (Result { state, node : Html state } [NotCached] -> { state, node : Html state }),
]

JsIndex : [
    JsIndex Nat, # Index of the corresponding real DOM node in a JavaScript array
    NoJsIndex, # We don't have a JavaScript index for fresh nodes not yet diffed, or for static HTML
]

LazyCallback state : Result { state, node : Html state } [NotCached] -> { state, node : Html state }

Attribute state : [
    EventListener Str (List CyclicStructureAccessor) (Result (Handler state) Nat),
    HtmlAttr Str Str,
    DomProp Str (List U8),
    Style Str Str,
]

CyclicStructureAccessor : [
    ObjectField Str CyclicStructureAccessor,
    ArrayIndex Nat CyclicStructureAccessor,
    SerializableValue,
]

# If we are only exposing the functions then are we better off just turning everything into a Custom?
# At some point we need a common format anyway. Wrapper lambda is irrelevant for perf in context of an event.
Handler state := [
    Normal (state, List (List U8) -> Action state),
    Custom (state, List (List U8) -> { action : Action state, stopPropagation : Bool, preventDefault : Bool }),
]

# -------------------------------
#   VIEW FUNCTIONS
# -------------------------------
## Define an HTML Element
element : Str -> (List (Attribute state), List (Html state) -> Html state)
element = \tagName ->
    \attrs, children ->
        # While building the node tree, calculate the size of Str it will render to
        withTag = 2 * (3 + Str.countUtf8Bytes tagName)
        withAttrs = List.walk attrs withTag \acc, attr -> acc + attrSize attr
        totalSize = List.walk children withAttrs \acc, child -> acc + nodeSize child

        Element tagName NoJsIndex totalSize attrs children

text : Str -> Html state
text = \content -> Text NoJsIndex content

# TODO: causes stack overflow in compiler
# lazy : (Result { state, node : Html state } [NotCached] -> { state, node : Html state }) -> Html state
# lazy = \f -> Lazy f
none : Html state
none = None

nodeSize : Html state -> Nat
nodeSize = \node ->
    when node is
        Text _ content -> Str.countUtf8Bytes content
        Element _ _ size _ _ -> size
        Lazy _ -> 0 # Ignore Lazy for buffer size estimate. renderStatic might have to reallocate, but that's OK.
        None -> 0

attrSize : Attribute state -> Nat
attrSize = \attr ->
    when attr is
        EventListener _ _ _ -> 0
        HtmlAttr key value -> 4 + Str.countUtf8Bytes key + Str.countUtf8Bytes value
        DomProp _ _ -> 0
        Style key value -> 4 + Str.countUtf8Bytes key + Str.countUtf8Bytes value

# -------------------------------
#   STATIC HTML
# -------------------------------
appendRenderedStatic : Str, Html [] -> Str
appendRenderedStatic = \buffer, node ->
    when node is
        Text _ content ->
            Str.concat buffer content

        Element name _ _ attrs children ->
            withTagName = "\(buffer)<\(name)"
            withAttrs =
                if List.isEmpty attrs then
                    withTagName
                else
                    init = { buffer: Str.concat withTagName " ", styles: "" }
                    { buffer: attrBuffer, styles } =
                        List.walk attrs init appendRenderedStaticAttr

                    if Str.isEmpty styles then
                        attrBuffer
                    else
                        "\(attrBuffer) style=\"\(styles)\""

            withTag = Str.concat withAttrs ">"
            withChildren = List.walk children withTag appendRenderedStatic

            "\(withChildren)</\(name)>"

        # Lazy can only be constructed in virtual DOM, not static
        None -> buffer

appendRenderedStaticAttr : { buffer : Str, styles : Str }, Attribute [] -> { buffer : Str, styles : Str }
appendRenderedStaticAttr = \{ buffer, styles }, attr ->
    when attr is
        HtmlAttr key value ->
            newBuffer = "\(buffer) \(key)=\"\(value)\""

            { buffer: newBuffer, styles }

        Style key value ->
            newStyles = "\(styles) \(key): \(value);"

            { buffer, styles: newStyles }

        # The remaining variants only make sense on the front end. Ignore for server-side rendering.
        EventListener _ _ _ -> { buffer, styles }
        DomProp _ _ -> { buffer, styles }

# -------------------------------
#   TRANSLATE STATE TYPE
# -------------------------------
# translate : Html c, (p -> c), (c -> p) -> Html p # TODO: use this type signature when it no longer triggers a type checker bug
translate : Html _, (_ -> _), (_ -> _) -> Html _
translate = \node, parentToChild, childToParent ->
    when node is
        Text jsIndex content ->
            Text jsIndex content

        Element name jsIndex size attrs children ->
            newAttrs = List.map attrs \a -> translateAttr a parentToChild childToParent
            newChildren = List.map children \c -> translate c parentToChild childToParent

            Element name jsIndex size newAttrs newChildren

        Lazy childCallback ->
            Lazy (translateLazy childCallback parentToChild childToParent)

        None -> None

# translateLazy : LazyCallback c, (p -> c), (c -> p) -> LazyCallback p # TODO: use this type signature when it no longer triggers a type checker bug
translateLazy : LazyCallback _, (_ -> _), (_ -> _) -> LazyCallback _
translateLazy = \childCallback, parentToChild, childToParent ->
    \parentCacheValue ->
        childCacheValue =
            parentCacheValue
            |> Result.map \v -> { state: parentToChild v.state, node: translate v.node childToParent parentToChild }
        { node, state } = childCallback childCacheValue

        {
            node: translate node parentToChild childToParent,
            state: childToParent state,
        }

# translateAttr : Attribute c, (p -> c), (c -> p) -> Attribute p
translateAttr : Attribute _, (_ -> _), (_ -> _) -> Attribute p
translateAttr = \attr, parentToChild, childToParent ->
    when attr is
        EventListener eventName accessors (Ok childHandler) ->
            EventListener eventName accessors (Ok (translateHandler childHandler parentToChild childToParent))

        EventListener eventName accessors (Err handlerId) ->
            EventListener eventName accessors (Err handlerId)

        HtmlAttr k v -> HtmlAttr k v
        DomProp k v -> DomProp k v
        Style k v -> Style k v

# translateHandler : Handler c, (p -> c), (c -> p) -> Handler p
translateHandler : Handler _, (_ -> _), (_ -> _) -> Handler _
translateHandler = \childHandler, parentToChild, childToParent ->
    when childHandler is
        @Handler (Normal childFn) ->
            parentFn = \parentState, jsons ->
                parentState |> parentToChild |> childFn jsons |> Action.map childToParent

            @Handler (Normal parentFn)

        @Handler (Custom childFn) ->
            parentFn = \parentState, jsons ->
                { action, stopPropagation, preventDefault } = childFn (parentToChild parentState) jsons

                { action: action |> Action.map childToParent, stopPropagation, preventDefault }

            @Handler (Custom parentFn)

translateStatic : Html state -> Html *
translateStatic = \node ->
    when node is
        Text jsIndex content ->
            Text jsIndex content

        Element name jsIndex size attrs children ->
            newAttrs = List.keepOks attrs keepStaticAttr
            newChildren = List.map children translateStatic

            Element name jsIndex size newAttrs newChildren

        # TODO: Triggers a stack overflow in the compiler. I think in type checking.
        # That's a pity because if someone used Lazy, it's probably worth server-side rendering.
        # Lazy callback ->
        #     { node: dynamicNode } = callback (Err NotCached)
        #     translateStatic dynamicNode
        Lazy _ -> None
        None -> None

keepStaticAttr : Attribute _ -> Result (Attribute *) {}
keepStaticAttr = \attr ->
    when attr is
        EventListener _ _ _ -> Err {}
        HtmlAttr k v -> Ok (HtmlAttr k v)
        DomProp _ _ -> Err {}
        Style k v -> Ok (Style k v)

# -------------------------------
#   EVENT HANDLING
# -------------------------------
JsEventResult state initData : {
    platformState : Box (PlatformState state initData),
    stopPropagation : Bool,
    preventDefault : Bool,
}

## Dispatch a JavaScript event to a Roc handler, given the handler ID and some JSON event data.
## We use Box to pass data structures on the Wasm heap. This is a lot easier than trying to access Wasm stack from JS.
## DANGER: this function does unusual stuff with memory allocation lifetimes. Be as careful as you would with Zig or C code!
dispatchEvent : Box (PlatformState state initData), Box (List (List U8)), Nat -> Effect (Box (JsEventResult state initData))
dispatchEvent = \boxedPlatformState, boxedEventData, handlerId ->
    { app, state, views, handlers } =
        Box.unbox boxedPlatformState
    eventData =
        Box.unbox boxedEventData
    maybeHandler =
        List.get handlers handlerId
        |> Result.withDefault (Err NoHandler)
    { action, stopPropagation, preventDefault } =
        when maybeHandler is
            Err NoHandler ->
                { action: Action.none, stopPropagation: Bool.false, preventDefault: Bool.false }

            Ok (@Handler (Normal handler)) ->
                { action: handler state eventData, stopPropagation: Bool.false, preventDefault: Bool.false }

            Ok (@Handler (Custom handler)) ->
                handler state eventData

    when action is
        Update newState ->
            # Switch to an arena allocator. All values allocated in the arena will be freed after the next update.
            _ <- Effect.enableVdomAllocator |> Effect.after
            newViews = app.renderDynamic newState
            emptyHandlers = List.repeat (Err NoHandler) (List.len handlers)

            newHandlers <- diffAndUpdateDom emptyHandlers views newViews |> Effect.after
            newPlatformState = Box.box {
                app,
                state: newState,
                views: newViews,
                handlers: newHandlers,
            }
            jsEventResult = Box.box { platformState: newPlatformState, stopPropagation, preventDefault }

            # Drop the arena for the previous update, and switch back to the normal allocator
            _ <- Effect.disableVdomAllocator |> Effect.after
            Effect.always jsEventResult

        # TODO: Roc compiler tells me I need a `_` pattern but I think I should just need `None`
        _ ->
            Effect.always (Box.box { platformState: boxedPlatformState, stopPropagation, preventDefault })

diffAndUpdateDom : HandlerLookup state, Dict HtmlId (Html state), Dict HtmlId (Html state) -> Effect (HandlerLookup state)

insertHandler : List (Result (Handler state) [NoHandler]), Handler state -> { index : Nat, handlers : List (Result (Handler state) [NoHandler]) }
insertHandler = \handlers, newHandler ->
    when List.findFirstIndex handlers Result.isErr is
        Ok index ->
            {
                index,
                handlers: List.set handlers index (Ok newHandler),
            }

        Err NotFound ->
            {
                index: List.len handlers,
                handlers: List.append handlers (Ok newHandler),
            }

replaceHandler : List (Result (Handler state) [NoHandler]), Nat, Handler state -> List (Result (Handler state) [NoHandler])
replaceHandler = \handlers, index, newHandler ->
    { list } = List.replace handlers index (Ok newHandler)

    list

# -------------------------------
#   SERVER SIDE INIT
# -------------------------------
initServerApp : initData, App state initData -> Result (Html []) [MissingHtmlIds (List Str), InvalidDocument] | initData has Encoding
initServerApp = \initData, app ->
    views =
        initData
        |> app.initDynamic
        |> app.renderDynamic
        |> Dict.map translateStatic

    { views: remainingViews, siblings } =
        populateViewContainers { views, siblings: [] } app.static

    if Dict.len remainingViews != 0 then
        Err (MissingHtmlIds (Dict.keys remainingViews))
    else
        when List.first siblings is
            Err _ ->
                # error is impossible, since we know the List has exactly one entry
                Err (MissingHtmlIds [])

            Ok document ->
                insertRocScript document initData (Dict.keys views) app.wasmUrl

insertRocScript : Html [], initData, List HtmlId, Str -> Result (Html []) [InvalidDocument] | initData has Encoding
insertRocScript = \document, initData, viewIds, wasmUrl ->
    # Convert initData to JSON as a Roc Str, then convert the Roc Str to a JS string.
    # JSON won't have invalid UTF-8 in it, since it would be escaped as part of JSON encoding.
    jsInitData =
        initData
        |> Encode.toBytes Json.toUtf8
        |> Str.fromUtf8
        |> Encode.toBytes Json.toUtf8
        |> Str.fromUtf8
        |> Result.withDefault ""
    jsViewIds = viewIds |> Encode.toBytes Json.toUtf8 |> Str.fromUtf8 |> Result.withDefault ""
    jsWasmUrl = wasmUrl |> Encode.toBytes Json.toUtf8 |> Str.fromUtf8 |> Result.withDefault ""

    script : Html []
    script = (element "script") [] [
        text
            """
            (function(){
            \(hostJavaScript)
            const initData = \(jsInitData);
            const viewIds = \(jsViewIds);
            const wasmUrl = \(jsWasmUrl);
            window.roc = roc_init(initData, viewIds, wasmUrl);
            })();
            """,
    ]

    # append the <script> to the end of the <body>
    when document is
        Element "html" hIndex hSize hAttrs hChildren ->
            empty = List.withCapacity (List.len hChildren)
            walkResult =
                List.walk hChildren { newHtmlChildren: empty, foundBody: Bool.false } \{ newHtmlChildren, foundBody }, hChild ->
                    when hChild is
                        Element "body" bIndex bSize bAttrs bChildren ->
                            {
                                newHtmlChildren: List.append newHtmlChildren (Element "body" bIndex bSize bAttrs (List.append bChildren script)),
                                foundBody: Bool.true,
                            }

                        _ ->
                            {
                                newHtmlChildren: List.append newHtmlChildren hChild,
                                foundBody,
                            }

            if walkResult.foundBody then
                Ok (Element "html" hIndex hSize hAttrs walkResult.newHtmlChildren)
            else
                Err InvalidDocument

        _ -> Err InvalidDocument

populateViewContainers : { views : Dict HtmlId (Html []), siblings : List (Html []) }, Html [] -> { views : Dict HtmlId (Html []), siblings : List (Html []) }
populateViewContainers = \walkState, oldTreeNode ->
    when oldTreeNode is
        Element name jsIndex size attrs children ->
            { views, siblings } = walkState

            maybeFound =
                List.walkUntil attrs (Err KeyNotFound) \_, attr ->
                    when attr is
                        HtmlAttr "id" id ->
                            Dict.get views id
                            |> Result.map \view -> { view, id }
                            |> Break

                        _ -> Err KeyNotFound |> Continue

            when maybeFound is
                Ok { view, id } ->
                    {
                        views: Dict.remove views id,
                        siblings: List.append siblings (Element name jsIndex size attrs [view]),
                    }

                Err KeyNotFound ->
                    emptyNewChildren = List.withCapacity (List.len children)
                    { views: newViews, siblings: newChildren } =
                        List.walk children { views, siblings: emptyNewChildren } populateViewContainers

                    {
                        views: newViews,
                        siblings: List.append siblings (Element name jsIndex size attrs newChildren),
                    }

        _ ->
            walkState

# -------------------------------
#   CLIENT SIDE INIT
# -------------------------------
ClientInit state : {
    state,
    dynamicViews : Dict HtmlId (Html state),
    staticViews : Dict HtmlId (Html state),
}

initClientApp : List U8, List Str, App state initData -> Result (ClientInit state) [JsonError, ViewNotFound HtmlId, UnusedViews (List HtmlId)] | initData has Decoding
initClientApp = \json, viewIdList, app ->
    initData <- json |> Decode.fromBytes Json.fromUtf8 |> Result.mapErr (\_ -> JsonError) |> Result.try
    state = app.initDynamic initData
    dynamicViews = app.renderDynamic state
    staticUnindexed = Dict.map dynamicViews translateStatic
    empty = Dict.withCapacity (Dict.len staticUnindexed)

    staticViews <- indexViews viewIdList staticUnindexed 0 0 empty |> Result.try
    Ok {
        state,
        dynamicViews,
        staticViews,
    }

indexViews : List HtmlId, Dict HtmlId (Html state), Nat, Nat, Dict HtmlId (Html state) -> Result (Dict HtmlId (Html state)) [ViewNotFound HtmlId, UnusedViews (List HtmlId)]
indexViews = \viewIdList, unindexedViews, viewIndex, nodeIndex, indexedViews ->
    when List.get viewIdList viewIndex is
        Ok id ->
            view <- Dict.get unindexedViews id |> Result.mapErr (\_ -> ViewNotFound id) |> Result.try
            indexedState = indexNodes { list: List.withCapacity 1, index: nodeIndex } view

            indexedView <- List.first indexedState.list |> Result.mapErr (\_ -> ViewNotFound id) |> Result.try
            newIndexedViews = Dict.insert indexedViews id indexedView
            newUnindexedViews = Dict.remove unindexedViews id

            indexViews viewIdList newUnindexedViews (viewIndex + 1) indexedState.index newIndexedViews

        Err OutOfBounds ->
            if Dict.len unindexedViews == 0 then
                Ok indexedViews
            else
                Err (UnusedViews (Dict.keys unindexedViews))

indexNodes : { list : List (Html state), index : Nat }, Html state -> { list : List (Html state), index : Nat }
indexNodes = \{ list, index }, node ->
    when node is
        Text jsIndex content ->
            { nodeIndex, nextIndex } =
                when jsIndex is
                    JsIndex id -> { nodeIndex: JsIndex id, nextIndex: index }
                    NoJsIndex -> { nodeIndex: JsIndex index, nextIndex: index + 1 }

            {
                list: List.append list (Text nodeIndex content),
                index: nextIndex,
            }

        Element name jsIndex size attrs children ->
            { list: newChildren, index: afterChildren } =
                List.walk children { list, index } indexNodes
            { nodeIndex, nextIndex } =
                when jsIndex is
                    JsIndex id -> { nodeIndex: JsIndex id, nextIndex: afterChildren }
                    NoJsIndex -> { nodeIndex: JsIndex afterChildren, nextIndex: afterChildren + 1 }

            {
                list: List.append list (Element name nodeIndex size attrs newChildren),
                index: nextIndex,
            }

        _ ->
            {
                list: List.append list node,
                index,
            }

# -------------------------------
#   VIRTUAL DOM DIFF
# -------------------------------
