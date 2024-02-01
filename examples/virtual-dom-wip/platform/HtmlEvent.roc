interface HtmlEvent
    exposes [
        Handler,
        CyclicStructureAccessor,
        on,
        custom,
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
        onInput,
        onSubmit,
    ]

import Action exposing [Action]
import HtmlInternalShared exposing [Attribute]

Handler state : HtmlInternalShared.Handler state
CyclicStructureAccessor : HtmlInternalShared.CyclicStructureAccessor

custom : Str, List CyclicStructureAccessor, (state, List (List U8) -> { action : Action state, stopPropagation : Bool, preventDefault : Bool }) -> Attribute state
custom = \eventName, accessors, callback ->
    EventListener eventName accessors (Custom callback)

on : Str, List CyclicStructureAccessor, (state, List (List U8) -> Action state) -> Attribute state
on = \eventName, accessors, callback ->
    EventListener eventName accessors (Normal callback)

# Internal helper
curriedOn : Str -> (List CyclicStructureAccessor, (state, List (List U8) -> Action state) -> Attribute state)
curriedOn = \eventName ->
    \accessors, callback ->
        EventListener eventName accessors (Normal callback)

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

onInput : List CyclicStructureAccessor, (state, List (List U8) -> Action state) -> Attribute state
onInput = \accessors, callback ->
    customCallback : state, List (List U8) -> { action : Action state, stopPropagation : Bool, preventDefault : Bool }
    customCallback = \state, jsons -> {
        action: callback state jsons,
        stopPropagation: Bool.true,
        preventDefault: Bool.false,
    }

    EventListener "input" accessors (Custom customCallback)

onSubmit : List CyclicStructureAccessor, (state, List (List U8) -> Action state) -> Attribute state
onSubmit = \accessors, callback ->
    customCallback = \state, jsons -> {
        action: callback state jsons,
        stopPropagation: Bool.false,
        preventDefault: Bool.true,
    }

    EventListener "submit" accessors (Custom customCallback)

# Notes from Elm:
#  - stopPropagation causes immediate view update, without waiting for animationFrame,
#       to prevent input state getting out of sync with model state when typing fast.
#  - Security-sensitive events trigger an immediate update within the same user-instigated tick
