app "hello-world"
    packages { pf: "platform" }
    imports []
    provides [ main ] to pf

Action state : [ None, Update state ]

Elem state :
    [
        Button (ButtonConfig state) (Elem state),
        Text Str,
        Col (List (Elem state)),
        Row (List (Elem state)),
    ]

ButtonConfig state : { onPress : PressEvent -> Action state }

PressEvent : { button : [ Touch, Mouse [ Left, Right, Middle ] ] }

none : Action *
none = None

button : { onPress : PressEvent -> Action state }, Elem state -> Elem state
button = \config, label ->
    Button config label

text : Str -> Elem *
text = \str ->
    Text str

main =
    btn = Button { onPress : \_ -> none } (text "Hello, button!")

    "Hello, World!\n"
