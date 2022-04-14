interface Game
    exposes [ Bounds, Elem, Event ]
    imports []

Rgba : { r : F32, g : F32, b : F32, a : F32 }

Bounds : { height : F32, width: F32 }

Elem : [ Rect { color : Rgba, left : F32, top : F32, width : F32, height : F32 }, Text Str ]

Event : [ Resize { width : F32, height : F32 }, KeyDown U32, KeyUp U32 ]

