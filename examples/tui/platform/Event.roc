interface Event
    exposes [ 
        KeyCode, 
        Bounds,
        Event,
        MediaKeyCode,
        ModifierKeyCode,
    ]
    imports []

Event : [
    KeyPressed KeyCode,
    FocusGained,
    FocusLost,
    Paste Str,
    Resize Bounds, 
]

Bounds : { height : U16, width : U16 }

KeyCode : [
    KeyBackspace,
    KeyEnter,
    KeyLeft,
    KeyRight,
    KeyUp,
    KeyDown,
    KeyHome,
    KeyEnd,
    KeyPageUp,
    KeyPageDown,
    KeyTab,
    KeyBackTab,
    KeyDelete,
    KeyInsert,
    KeyFunction U8,
    KeyScalar U32,
    KeyNull,
    KeyEsc,
    KeyCapsLock,
    KeyScrollLock,
    KeyNumLock,
    KeyPrintScreen,
    KeyPause,
    KeyMenu,
    KeyKeypadBegin,
    KeyMedia MediaKeyCode,
    KeyModifier ModifierKeyCode,
]

MediaKeyCode : [
    KeyPlay,
    KeyPause,
    KeyPlayPause,
    KeyReverse,
    KeyStop,
    KeyFastForward,
    KeyRewind,
    KeyTrackNext,
    KeyTrackPrevious,
    KeyRecord,
    KeyLowerVolume,
    KeyRaiseVolume,
    KeyMuteVolume,
]

ModifierKeyCode : [
    KeyLeftShift,
    KeyLeftControl,
    KeyLeftAlt,
    KeyLeftSuper,
    KeyLeftHyper,
    KeyLeftMeta,
    KeyRightShift,
    KeyRightControl,
    KeyRightAlt,
    KeyRightSuper,
    KeyRightHyper,
    KeyRightMeta,
    KeyIsoLevel3Shift,
    KeyIsoLevel5Shift,
]