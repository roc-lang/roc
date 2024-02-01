interface HtmlInternalShared
    exposes [
        App,
        Html,
        Attribute,
        CyclicStructureAccessor,
        Handler,
        Size,
        element,
        text,
        none,
        translate,
        translateStatic,
        nodeSize,
    ]

import Action exposing [Action]

App state initData : {
    init : DecodingResult initData -> state,
    render : state -> Html state,
    wasmUrl : Str,
}

DecodingResult a : Result a [Leftover (List U8), TooShort]

Html state : [
    None,
    Text Str,
    Element Str Size (List (Attribute state)) (List (Html state)),
]

# The pre-calculated byte size of the rendered HTML string
Size : Nat

Attribute state : [
    EventListener Str (List CyclicStructureAccessor) (Handler state),
    HtmlAttr Str Str,
    DomProp Str (List U8),
    Style Str Str,
]

CyclicStructureAccessor : [
    ObjectField Str CyclicStructureAccessor,
    ArrayIndex U32 CyclicStructureAccessor,
    SerializableValue,
]

# If we are only exposing the functions then are we better off just turning everything into a Custom?
# At some point we need a common format anyway. Wrapper lambda is irrelevant for perf in context of an event.
Handler state : [
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
        # While building the node nodes, calculate the size of Str it will render to
        withTag = 2 * (3 + Str.countUtf8Bytes tagName)
        withAttrs = List.walk attrs withTag \acc, attr -> acc + attrSize attr
        totalSize = List.walk children withAttrs \acc, child -> acc + nodeSize child

        Element tagName totalSize attrs children

text : Str -> Html state
text = \content -> Text content

none : Html state
none = None

nodeSize : Html state -> Nat
nodeSize = \node ->
    when node is
        Text content -> Str.countUtf8Bytes content
        Element _ size _ _ -> size
        None -> 0

attrSize : Attribute state -> Nat
attrSize = \attr ->
    when attr is
        EventListener _ _ _ -> 0
        HtmlAttr key value -> 4 + Str.countUtf8Bytes key + Str.countUtf8Bytes value
        DomProp _ _ -> 0
        Style key value -> 4 + Str.countUtf8Bytes key + Str.countUtf8Bytes value

# -------------------------------
#   TRANSLATE STATE TYPE
# -------------------------------
translate : Html c, (p -> c), (c -> p) -> Html p
translate = \node, parentToChild, childToParent ->
    when node is
        Text content ->
            Text content

        Element name size attrs children ->
            newAttrs = List.map attrs \a -> translateAttr a parentToChild childToParent
            newChildren = List.map children \c -> translate c parentToChild childToParent

            Element name size newAttrs newChildren

        None -> None

translateAttr : Attribute c, (p -> c), (c -> p) -> Attribute p
translateAttr = \attr, parentToChild, childToParent ->
    when attr is
        EventListener eventName accessors childHandler ->
            EventListener eventName accessors (translateHandler childHandler parentToChild childToParent)

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

translateStatic : Html state -> Html *
translateStatic = \node ->
    when node is
        Text content ->
            Text content

        Element name size attrs children ->
            newAttrs = List.keepOks attrs keepStaticAttr
            newChildren = List.map children translateStatic

            Element name size newAttrs newChildren

        None -> None

keepStaticAttr : Attribute _ -> Result (Attribute *) {}
keepStaticAttr = \attr ->
    when attr is
        EventListener _ _ _ -> Err {}
        HtmlAttr k v -> Ok (HtmlAttr k v)
        DomProp _ _ -> Err {}
        Style k v -> Ok (Style k v)
