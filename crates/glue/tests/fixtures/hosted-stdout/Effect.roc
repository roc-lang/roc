hosted Effect
    exposes [
        Effect,
        after, map, always,
        stdoutLine,
    ]
    imports []
    generates Effect with [after, map, always, forever, loop]

stdoutLine : Str -> Effect {}
