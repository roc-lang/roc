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
        insertHandler,
        replaceHandler,
        dispatchEvent,
        appendRenderedStatic,
        nodeSize,
    ]
    imports [Action.{ Action }, Encode, Json, Html.HostJavaScript.{ hostJavaScript }]

App state initData : {
    static : Html [],
    initDynamic : initData -> state,
    renderDynamic : state -> Dict HtmlId (Html state),
    wasmUrl : Str,
} | initData has Encoding

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

translateAttr : Attribute c, (p -> c), (c -> p) -> Attribute p
translateAttr = \attr, parentToChild, childToParent ->
    when attr is
        EventListener eventName accessors (Ok childHandler) ->
            EventListener eventName accessors (Ok (translateHandler childHandler parentToChild childToParent))

        EventListener eventName accessors (Err handlerId) ->
            EventListener eventName accessors (Err handlerId)

        HtmlAttr k v -> HtmlAttr k v
        DomProp k v -> DomProp k v
        Style k v -> Style k v

translateHandler : Handler c, (p -> c), (c -> p) -> Handler p
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
insertHandler : List (Result (Handler state) [NoHandler]), Handler state -> { index : Nat, lookup : List (Result (Handler state) [NoHandler]) }
insertHandler = \lookup, newHandler ->
    when List.findFirstIndex lookup Result.isErr is
        Ok index ->
            {
                index,
                lookup: List.set lookup index (Ok newHandler),
            }

        Err NotFound ->
            {
                index: List.len lookup,
                lookup: List.append lookup (Ok newHandler),
            }

replaceHandler : List (Result (Handler state) [NoHandler]), Nat, Handler state -> List (Result (Handler state) [NoHandler])
replaceHandler = \lookup, index, newHandler ->
    { list } = List.replace lookup index (Ok newHandler)

    list

dispatchEvent : List (Result (Handler state) [NoHandler]), Nat, List (List U8), state -> { action : Action state, stopPropagation : Bool, preventDefault : Bool }
dispatchEvent = \lookup, handlerId, eventData, state ->
    maybeHandler =
        List.get lookup handlerId
        |> Result.withDefault (Err NoHandler)

    when maybeHandler is
        Err NoHandler ->
            { action: Action.none, stopPropagation: Bool.false, preventDefault: Bool.false }

        Ok (@Handler (Normal handler)) ->
            action = handler state eventData

            { action, stopPropagation: Bool.false, preventDefault: Bool.false }

        Ok (@Handler (Custom handler)) ->
            handler state eventData

# -------------------------------
#   SERVER SIDE INIT
# -------------------------------
initServerApp : initData, App state initData -> Result (Html []) [MissingHtmlIds (List Str), InvalidDocument]*
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

insertRocScript : Html [], initData, List HtmlId, Str -> Result (Html []) [InvalidDocument]* | initData has Encoding
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
