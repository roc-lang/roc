interface Html.Internal
    exposes [
        Node,
        Attribute,
        CyclicStructureAccessor,
        Handler,
        translate,
        insertHandler,
        replaceHandler,
        dispatchEvent,
    ]
    imports [Action.{ Action }]

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

# This when expression produces:
#
#     [Element Str Nat (List (Attribute p ? ?)) (List [
#           Element Str Nat (List (Attribute c ? ?)) (List ∞),
#           Lazy (Result { node : ∞, state : c } [NotCached] -> { node : ∞, state : c }),
#           None,
#           Text Str] as ∞),
#     Lazy (LazyCallback p ? ? ? ? ? ? ?), None, Text Str]
#
# But you are trying to use it as:
#
#     [Element Str Nat (List (Attribute p ? ?)) (List b), Lazy (Result {
#     node : Node p ? ? ?, state : p } [NotCached] -> { node : b,
#     state : p }), None, Text Str] as b
#
translate : Node c, (p -> c), (c -> p) -> Node p
translate = \node, parentToChild, childToParent ->
    when node is
        Text content ->
            Text content

        Element name size attrs children ->
            newAttrs = List.map attrs \a -> translateAttr a parentToChild childToParent
            # why does this have the wrong type?
            newChildren = List.map children \c -> translate c parentToChild childToParent

            Element name size newAttrs newChildren

        Lazy childCallback ->
            Lazy (translateLazy childCallback parentToChild childToParent)

        None -> None

translateLazy : LazyCallback c, (p -> c), (c -> p) -> LazyCallback p
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
