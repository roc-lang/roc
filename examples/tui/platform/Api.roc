interface Api
    exposes [
        Elem, 
        KeyCode, 
        Event,
        Bounds, 
    ]
    imports []

Elem : [
    Text { text : Str },
]

Bounds : { height : F32, width : F32 }

KeyCode : [
    Left, 
    Right,
    Other, 
    Up, 
    Down,
]

Event : [
    Resize { width : F32, height : F32 }, 
    KeyDown KeyCode, 
    KeyUp KeyCode, 
    Tick U128,
]
