hosted Effect
    exposes [Effect, after, map, always, forever, loop, putLine, putInt, getInt, InternalTypeId, newInternalTypeId]
    imports []
    generates Effect with [after, map, always, forever, loop]

InternalTypeId := U32
     has [ Eq, Hash ]

newInternalTypeId = @InternalTypeId

putLine : Str -> Effect {}

putInt : I64 -> Effect {}

getInt : Effect { value : I64, isError : Bool }
