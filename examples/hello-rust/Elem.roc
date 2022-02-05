interface Elem
    exposes [ Elem, PressEvent, row, col, text, button ]
    imports [ Action.{ Action }]

Elem state :
    [
        Button (ButtonConfig state) (Elem state),
        Text Str,
        Col (List (Elem state)),
        Row (List (Elem state)),
        Lazy2 (forall a, b | { a, b, state, lambda: a, b -> Elem state })
        Lazy2<a, b> { a, b, state, lambda: a, b -> Elem state }

        Lazy2 : a, b, state, (a, b -> Elem state) -> Elem state
    ]

ButtonConfig state : { onPress : PressEvent -> Action state }

PressEvent : { button : [ Touch, Mouse [ Left, Right, Middle ] ] }

text : Str -> Elem *
text = \str ->
    Text str

button : { onPress : PressEvent -> Action state }, Elem state -> Elem state
button = \config, label ->
    Button config label

row : List (Elem state) -> Elem state
row = \children ->
    Row children

col : List (Elem state) -> Elem state
col = \children ->
    Col children

map : Elem a, (a -> b) -> Elem b
map = \child, transform ->
    when child is
        Button config label ->
            onPress = \event ->
                config.onPress event
                    |> Action.map transform

            Button { onPress } (map label transform)
        Text str -> Text str
        Col children -> Col (List.map children \c -> map c transform)
        Row children -> Row (List.map children \c -> map c transform)
