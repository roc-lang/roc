hosted Effect
    exposes [Effect, after, map, always, forever, loop, putLine, putInt, getInt]
    generates Effect with [after, map, always, forever, loop]

putLine : Str -> Effect {}

putInt : I64 -> Effect {}

getInt : Effect { value : I64, isError : Bool }
