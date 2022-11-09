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
    Backspace,
    Enter,
    Left,
    Right,
    Up,
    Down,
    Home,
    End,
    PageUp,
    PageDown,
    Tab,
    BackTab,
    Delete,
    Insert,
    Function U8,
    Scalar Str,
    Null,
    Esc,
    CapsLock,
    ScrollLock,
    NumLock,
    PrintScreen,
    Pause,
    Menu,
    KeypadBegin,
    Media MediaKeyCode,
    Modifier ModifierKeyCode,
]

MediaKeyCode : [
    Play,
    Pause,
    PlayPause,
    Reverse,
    Stop,
    FastForward,
    Rewind,
    TrackNext,
    TrackPrevious,
    Record,
    LowerVolume,
    RaiseVolume,
    MuteVolume,
]

ModifierKeyCode : [
    LeftShift,
    LeftControl,
    LeftAlt,
    LeftSuper,
    LeftHyper,
    LeftMeta,
    RightShift,
    RightControl,
    RightAlt,
    RightSuper,
    RightHyper,
    RightMeta,
    IsoLevel3Shift,
    IsoLevel5Shift,
]