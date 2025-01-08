module [
    Handler,
    CyclicStructureAccessor,
    on,
    custom,
    on_click,
    on_double_click,
    on_mouse_down,
    on_mouse_up,
    on_mouse_enter,
    on_mouse_leave,
    on_mouse_over,
    on_mouse_out,
    on_check,
    on_blur,
    on_focus,
    on_input,
    on_submit,
]

import Action exposing [Action]
import Html.Internal.Shared exposing [Attribute]

Handler state : Html.Internal.Shared.Handler state
CyclicStructureAccessor : Html.Internal.Shared.CyclicStructureAccessor

custom : Str, List CyclicStructureAccessor, (state, List (List U8) -> { action : Action state, stop_propagation : Bool, prevent_default : Bool }) -> Attribute state
custom = \event_name, accessors, callback ->
    EventListener(event_name, accessors, Custom(callback))

on : Str, List CyclicStructureAccessor, (state, List (List U8) -> Action state) -> Attribute state
on = \event_name, accessors, callback ->
    EventListener(event_name, accessors, Normal(callback))

# Internal helper
curried_on : Str -> (List CyclicStructureAccessor, (state, List (List U8) -> Action state) -> Attribute state)
curried_on = \event_name ->
    \accessors, callback ->
        EventListener(event_name, accessors, Normal(callback))

on_click = curried_on("click")
on_double_click = curried_on("dblclick")
on_mouse_down = curried_on("mousedown")
on_mouse_up = curried_on("mouseup")
on_mouse_enter = curried_on("mouseenter")
on_mouse_leave = curried_on("mouseleave")
on_mouse_over = curried_on("mouseover")
on_mouse_out = curried_on("mouseout")
on_check = curried_on("check")
on_blur = curried_on("blur")
on_focus = curried_on("focus")

on_input : List CyclicStructureAccessor, (state, List (List U8) -> Action state) -> Attribute state
on_input = \accessors, callback ->
    custom_callback : state, List (List U8) -> { action : Action state, stop_propagation : Bool, prevent_default : Bool }
    custom_callback = \state, jsons -> {
        action: callback(state, jsons),
        stop_propagation: Bool.true,
        prevent_default: Bool.false,
    }

    EventListener("input", accessors, Custom(custom_callback))

on_submit : List CyclicStructureAccessor, (state, List (List U8) -> Action state) -> Attribute state
on_submit = \accessors, callback ->
    custom_callback = \state, jsons -> {
        action: callback(state, jsons),
        stop_propagation: Bool.false,
        prevent_default: Bool.true,
    }

    EventListener("submit", accessors, Custom(custom_callback))

# Notes from Elm:
#  - stopPropagation causes immediate view update, without waiting for animationFrame,
#       to prevent input state getting out of sync with model state when typing fast.
#  - Security-sensitive events trigger an immediate update within the same user-instigated tick
