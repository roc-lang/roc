interface Game
    exposes [Bounds, Elem, Event]
    imports []

Rgba : { r : F32, g : F32, b : F32, a : F32 }

Bounds : { height : F32, width : F32 }

Elem : [Rect { color : Rgba, left : F32, top : F32, width : F32, height : F32 }, Text { text : Str, color : Rgba, left : F32, top : F32, size : F32 }]

KeyCode : [Left, Right, Other, Up, Down]

Event : [Resize { width : F32, height : F32 }, KeyDown KeyCode, KeyUp KeyCode, Tick U128]
