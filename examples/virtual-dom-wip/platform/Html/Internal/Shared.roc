module [
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
    translate_static,
    node_size,
]

import Action exposing [Action]

App state init_data : {
    init : DecodingResult init_data -> state,
    render : state -> Html state,
    wasm_url : Str,
}

DecodingResult a : Result a [Leftover (List U8), TooShort]

Html state : [
    None,
    Text Str,
    Element Str Size (List (Attribute state)) (List (Html state)),
]

# The pre-calculated byte size of the rendered HTML string
Size : U64

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
    Custom (state, List (List U8) -> { action : Action state, stop_propagation : Bool, prevent_default : Bool }),
]

# -------------------------------
#   VIEW FUNCTIONS
# -------------------------------
## Define an HTML Element
element : Str -> (List (Attribute state), List (Html state) -> Html state)
element = \tag_name ->
    \attrs, children ->
        # While building the node nodes, calculate the size of Str it will render to
        with_tag = 2 * (3 + Str.count_utf8_bytes(tag_name))
        with_attrs = List.walk(attrs, with_tag, \acc, attr -> acc + attr_size(attr))
        total_size = List.walk(children, with_attrs, \acc, child -> acc + node_size(child))

        Element(tag_name, total_size, attrs, children)

text : Str -> Html state
text = \content -> Text(content)

none : Html state
none = None

node_size : Html state -> U64
node_size = \node ->
    when node is
        Text(content) -> Str.count_utf8_bytes(content)
        Element(_, size, _, _) -> size
        None -> 0

attr_size : Attribute state -> U64
attr_size = \attr ->
    when attr is
        EventListener(_, _, _) -> 0
        HtmlAttr(key, value) -> 4 + Str.count_utf8_bytes(key) + Str.count_utf8_bytes(value)
        DomProp(_, _) -> 0
        Style(key, value) -> 4 + Str.count_utf8_bytes(key) + Str.count_utf8_bytes(value)

# -------------------------------
#   TRANSLATE STATE TYPE
# -------------------------------
translate : Html c, (p -> c), (c -> p) -> Html p
translate = \node, parent_to_child, child_to_parent ->
    when node is
        Text(content) ->
            Text(content)

        Element(name, size, attrs, children) ->
            new_attrs = List.map(attrs, \a -> translate_attr(a, parent_to_child, child_to_parent))
            new_children = List.map(children, \c -> translate(c, parent_to_child, child_to_parent))

            Element(name, size, new_attrs, new_children)

        None -> None

translate_attr : Attribute c, (p -> c), (c -> p) -> Attribute p
translate_attr = \attr, parent_to_child, child_to_parent ->
    when attr is
        EventListener(event_name, accessors, child_handler) ->
            EventListener(event_name, accessors, translate_handler(child_handler, parent_to_child, child_to_parent))

        HtmlAttr(k, v) -> HtmlAttr(k, v)
        DomProp(k, v) -> DomProp(k, v)
        Style(k, v) -> Style(k, v)

translate_handler : Handler c, (p -> c), (c -> p) -> Handler p
translate_handler = \child_handler, parent_to_child, child_to_parent ->
    when child_handler is
        Normal(child_fn) ->
            parent_fn = \parent_state, jsons ->
                parent_state |> parent_to_child |> child_fn(jsons) |> Action.map(child_to_parent)

            Normal(parent_fn)

        Custom(child_fn) ->
            parent_fn = \parent_state, jsons ->
                { action, stop_propagation, prevent_default } = child_fn(parent_to_child(parent_state), jsons)

                { action: action |> Action.map(child_to_parent), stop_propagation, prevent_default }

            Custom(parent_fn)

translate_static : Html state -> Html *
translate_static = \node ->
    when node is
        Text(content) ->
            Text(content)

        Element(name, size, attrs, children) ->
            new_attrs = List.keep_oks(attrs, keep_static_attr)
            new_children = List.map(children, translate_static)

            Element(name, size, new_attrs, new_children)

        None -> None

keep_static_attr : Attribute _ -> Result (Attribute *) {}
keep_static_attr = \attr ->
    when attr is
        EventListener(_, _, _) -> Err({})
        HtmlAttr(k, v) -> Ok(HtmlAttr(k, v))
        DomProp(_, _) -> Err({})
        Style(k, v) -> Ok(Style(k, v))
