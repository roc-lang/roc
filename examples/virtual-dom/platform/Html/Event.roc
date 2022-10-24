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

Handler state : {
    eventName : Str,
    accessors : List CyclicStructureAccessor,
    update : state, List (List U8) -> Action state,
}

on : Str -> (List CyclicStructureAccessor, (state, List (List U8) -> Action state) -> Handler state)
on = \eventName ->
    \accessors, update -> {
        eventName,
        accessors,
        update,
    }

onClick = on "click"
onDoubleClick = on "dblclick"
onMouseDown = on "mousedown"
onMouseUp = on "mouseup"
onMouseEnter = on "mouseenter"
onMouseLeave = on "mouseleave"
onMouseOver = on "mouseover"
onMouseOut = on "mouseout"
onCheck = on "check"
onBlur = on "blur"
onFocus = on "focus"

# Custom handlers will also want to return stopPropagation and preventDefault as well as an Action
# Notes from Elm:
#  - onInput handler always returns true for stopPropagation
#  - onSubmit handler always returns true for preventDefault
#  - stopPropagation causes immediate view update, without waiting for animationFrame,
#       to prevent input state getting out of sync with model state when typing fast.
#  - Security-sensitive events trigger an immediate update within the same user-instigated tick
