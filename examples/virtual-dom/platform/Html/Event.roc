interface Html.Event
    exposes [
        Handler,
        CyclicStructureAccessor,
        onClick,
        onDoubleClick,
        onMouseDown,
        onMouseUp,
        onMouseEnter,
        onMouseLeave,
        onMouseOver,
        onMouseOut,
        onCheck,
        onBlur,
        onFocus,
    ]
    imports [Action.{ Action }]

CyclicStructureAccessor : [
    ObjectField Str CyclicStructureAccessor,
    ArrayIndex Nat CyclicStructureAccessor,
    SerializableValue,
]

# If we are only exposing the functions then are we better off just turning everything into a Custom?
# At some point we need a common format anyway. Wrapper lambda is irrelevant for perf in context of an event.
Handler state  : [
    Normal (state, List (List U8) -> Action state),
    MayStopPropagation (state, List (List U8) -> { action: Action state, stopPropagation: Bool }),
    MayPreventDefault (state, List (List U8) -> { action: Action state, preventDefault: Bool }),
    Custom (state, List (List U8) -> { action: Action state, stopPropagation: Bool, preventDefault: Bool ),
]

insertHandler : List (Result (Handler state) NoHandler), Handler state -> { index : Nat, lookup: List (Result (Handler state) NoHandler) }
insertHandler = \lookup, newHandler ->
    when List.findFirstIndex lookup (\h -> h == Err NoHandler) is
        Ok index ->
            { index,
              lookup: List.set lookup index (Ok newHandler)
            }

        Err NotFound ->
            { index: List.length lookup,
              lookup: List.append lookup (Ok newHandler)
            }

replaceHandler : List (Result (Handler state) NoHandler), Nat, Handler state
replaceHandler = \lookup, index, newHandler ->
    { list } = List.replace lookup index newHandler
    list

dispatchEvent : List (Result (Handler state) NoHandler), Nat, List (List U8), state -> { action: Action state, stopPropagation: Bool, preventDefault: Bool }
dispatchEvent = \lookup, handlerId, eventData, state ->
    maybeHandler = List.get lookup handlerId |> Result.withDefault (Err NoHandler)
    when maybeHandler is
        Err NoHandler ->
            { action: Action.none, stopPropagation: Bool.false, preventDefault: Bool.false }

        Ok (Normal handler) ->
            action = handler eventData state
            { action, stopPropagation: Bool.false, preventDefault: Bool.false }

        Ok (MayStopPropagation handler) ->
            { action, stopPropagation } = handler eventData state
            { action, stopPropagation, preventDefault: Bool.false }

        Ok (MayPreventDefault handler) ->
            { action, preventDefault } = handler eventData state
            { action, stopPropagation: Bool.false, preventDefault }

        Ok (Custom handler) ->
            handler eventData state

on : Str, (List CyclicStructureAccessor, (state, List (List U8) -> Action state) -> Handler state)
on = \eventName, accessors, callback -> {
    eventName,
    accessors,
    callback: Normal callback,
}

# Internal helper
curriedOn : Str -> (List CyclicStructureAccessor, (state, List (List U8) -> Action state) -> Handler state)
curriedOn = \eventName ->
    \accessors, callback -> {
        eventName,
        accessors,
        callback: Normal callback,
    }

stopPropgagationOn : Str -> (List CyclicStructureAccessor, (state, List (List U8) -> { action: Action state, stopPropagation: Bool }) -> Handler state)
stopPropgagationOn = \eventName, accessors, callback -> {
    eventName,
    accessors,
    callback: MayStopPropagation callback,
}

preventDefaultOn : Str -> (List CyclicStructureAccessor, (state, List (List U8) -> { action: Action state, preventDefault: Bool }) -> Handler state)
preventDefaultOn = \eventName, accessors, callback -> {
    eventName,
    accessors,
    callback: MayPreventDefault callback,
}

custom : Str, (List CyclicStructureAccessor, (state, List (List U8) -> { action: Action state, stopPropagation: Bool, preventDefault: Bool }) -> Handler state)
custom = \eventName, accessors, callback -> {
    eventName,
    accessors,
    callback: Custom callback,
}

onClick = curriedOn "click"
onDoubleClick = curriedOn "dblclick"
onMouseDown = curriedOn "mousedown"
onMouseUp = curriedOn "mouseup"
onMouseEnter = curriedOn "mouseenter"
onMouseLeave = curriedOn "mouseleave"
onMouseOver = curriedOn "mouseover"
onMouseOut = curriedOn "mouseout"
onCheck = curriedOn "check"
onBlur = curriedOn "blur"
onFocus = curriedOn "focus"

# Custom handlers will also want to return stopPropagation and preventDefault as well as an Action
# Notes from Elm:
#  - onInput handler always returns true for stopPropagation
#  - onSubmit handler always returns true for preventDefault
#  - stopPropagation causes immediate view update, without waiting for animationFrame,
#       to prevent input state getting out of sync with model state when typing fast.
#  - Security-sensitive events trigger an immediate update within the same user-instigated tick
