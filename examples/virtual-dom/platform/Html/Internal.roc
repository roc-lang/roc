interface Html.Internal
    exposes [
        Node,
        Attribute,
        CyclicStructureAccessor,
        Handler,
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
    maybeHandler = List.get lookup handlerId |> Result.withDefault (Err NoHandler)

    when maybeHandler is
        Err NoHandler ->
            { action: Action.none, stopPropagation: Bool.false, preventDefault: Bool.false }

        Ok (Normal handler) ->
            action = handler state eventData

            { action, stopPropagation: Bool.false, preventDefault: Bool.false }

        Ok (Custom handler) ->
            handler state eventData
