interface Api
    exposes [ 
        KeyCode, 
        Event,
        Bounds, 
    ]
    imports []

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
