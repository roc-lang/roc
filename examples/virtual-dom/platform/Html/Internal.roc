interface Html.Internal
    exposes [
        App,
        Node,
        Attribute,
        CyclicStructureAccessor,
        Handler,
        element,
        translate,
        translateStatic,
        insertHandler,
        replaceHandler,
        dispatchEvent,
        rocScript,
        appendRenderedStatic,
        nodeSize,
    ]
    imports [Action.{ Action }, Encode, Json, Html.HostJavaScript.{ hostJavaScript }]

Node state : [
    None,
    Text Str,
    Element Str Nat (List (Attribute state)) (List (Node state)),
    Lazy (Result { state, node : Node state } [NotCached] -> { state, node : Node state }),
]

LazyCallback state : Result { state, node : Node state } [NotCached] -> { state, node : Node state }

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
Handler state : [
    Normal (state, List (List U8) -> Action state),
    Custom (state, List (List U8) -> { action : Action state, stopPropagation : Bool, preventDefault : Bool }),
]

## Define an HTML Element
element : Str -> (List (Attribute state), List (Node state) -> Node state)
element = \tagName ->
    \attrs, children ->
        # While building the node tree, calculate the size of Str it will render to
        withTag = 2 * (3 + Str.countUtf8Bytes tagName)
        withAttrs = List.walk attrs withTag \acc, attr -> acc + attrSize attr
        totalSize = List.walk children withAttrs \acc, child -> acc + nodeSize child

        Element tagName totalSize attrs children

# internal helper
nodeSize : Node state -> Nat
nodeSize = \node ->
    when node is
        Text content -> Str.countUtf8Bytes content
        Element _ size _ _ -> size
        Lazy _ -> 0 # Ignore Lazy for buffer size estimate. renderStatic might have to reallocate, but that's OK.
        None -> 0

# internal helper
attrSize : Attribute state -> Nat
attrSize = \attr ->
    when attr is
        EventListener _ _ _ -> 0
        HtmlAttr key value -> 4 + Str.countUtf8Bytes key + Str.countUtf8Bytes value
        DomProp _ _ -> 0
        Style key value -> 4 + Str.countUtf8Bytes key + Str.countUtf8Bytes value

# internal helper
appendRenderedStatic : Str, Node [] -> Str
appendRenderedStatic = \buffer, node ->
    when node is
        Text content ->
            Str.concat buffer content

        Element name _ attrs children ->
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

# translate : Node c, (p -> c), (c -> p) -> Node p # TODO: use this type signature when it no longer triggers a type checker bug
translate : Node _, (_ -> _), (_ -> _) -> Node _
translate = \node, parentToChild, childToParent ->
    when node is
        Text content ->
            Text content

        Element name size attrs children ->
            newAttrs = List.map attrs \a -> translateAttr a parentToChild childToParent
            newChildren = List.map children \c -> translate c parentToChild childToParent

            Element name size newAttrs newChildren

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
        EventListener eventName accesors (Ok childHandler) ->
            EventListener eventName accesors (Ok (translateHandler childHandler parentToChild childToParent))

        EventListener eventName accesors (Err handlerId) ->
            EventListener eventName accesors (Err handlerId)

        HtmlAttr k v -> HtmlAttr k v
        DomProp k v -> DomProp k v
        Style k v -> Style k v

translateHandler : Handler c, (p -> c), (c -> p) -> Handler p
translateHandler = \childHandler, parentToChild, childToParent ->
    when childHandler is
        Normal childFn ->
            parentFn = \parentState, jsons ->
                parentState |> parentToChild |> childFn jsons |> Action.map childToParent

            Normal parentFn

        Custom childFn ->
            parentFn = \parentState, jsons ->
                { action, stopPropagation, preventDefault } = childFn (parentToChild parentState) jsons

                { action: action |> Action.map childToParent, stopPropagation, preventDefault }

            Custom parentFn

translateStatic : Node * -> Node []
translateStatic = \node ->
    when node is
        Text content ->
            Text content

        Element name size attrs children ->
            newAttrs = List.keepOks attrs keepStaticAttr
            newChildren = List.map children translateStatic

            Element name size newAttrs newChildren

        # TODO: Triggers a stack overflow in the compiler. I think in type checking.
        # If someone made this node Lazy, then it's probably expensive, and worth server-side rendering.
        # Lazy callback ->
        #     { node: dynamicNode } = callback (Err NotCached)
        #     translateStatic dynamicNode
        Lazy _ -> None
        None -> None

keepStaticAttr : Attribute _ -> Result (Attribute []) {}
keepStaticAttr = \attr ->
    when attr is
        EventListener _ _ _ -> Err {}
        HtmlAttr k v -> Ok (HtmlAttr k v)
        DomProp _ _ -> Err {}
        Style k v -> Ok (Style k v)

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

        Ok (Normal handler) ->
            action = handler state eventData

            { action, stopPropagation: Bool.false, preventDefault: Bool.false }

        Ok (Custom handler) ->
            handler state eventData

HtmlId : Str

App state : {
    static : Node [],
    initDynamic : Str -> state,
    renderDynamic : state -> Dict HtmlId (Node state),
}

rocScript : Str, List HtmlId, Str -> Result (Node []) [InvalidUtf8]*
rocScript = \initData, dynamicRootIds, wasmUrl ->
    toJs = \data ->
        data
        |> Encode.toBytes Json.toUtf8
        |> Str.fromUtf8
    encInitData = toJs initData
    encDynamicRootIds = toJs dynamicRootIds
    encWasmUrl = toJs wasmUrl

    when { encInitData, encDynamicRootIds, encWasmUrl } is
        { encInitData: Ok jsInitData, encDynamicRootIds: Ok jsDynamicRootIds, encWasmUrl: Ok jsWasmUrl } ->
            elem : Node []
            elem = (element "script") [] [
                Text
                    """
                    \(hostJavaScript)
                    (function() {
                        const initData = \(jsInitData);
                        const dynamicRootIds = \(jsDynamicRootIds);
                        const wasmUrl = \(jsWasmUrl);
                        window.roc.init(initData, dynamicRootIds, wasmUrl);
                    })();
                    """,
            ]

            Ok elem

        _ ->
            Err InvalidUtf8
