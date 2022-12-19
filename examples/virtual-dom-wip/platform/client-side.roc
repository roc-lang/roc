platform "client-side"
    requires {} { app : App state initData }
    exposes []
    packages {}
    imports [
        Html.Internal.{ App, JsEventResult, PlatformState },
        Effect.{ Effect },
    ]
    provides [main]

# Fields sorted by alignment, then alphabetically
FromHost state initData : {
    eventHandlerId : Nat,
    eventJsonList : List (List U8),
    eventPlatformState : Box (PlatformState state initData),
    initJson : List U8,
    isInitEvent : Bool,
}

# Fields sorted by alignment, then alphabetically
ToHost state initData : {
    platformState : Box (PlatformState state initData),
    eventPreventDefault : Bool,
    eventStopPropagation : Bool,
}

# Note: named type variables cause a type error here!
main : FromHost _ _ -> Effect (ToHost _ _)
main = \{ eventHandlerId, eventJsonList, eventPlatformState, initJson, isInitEvent } ->
    if isInitEvent then
        Html.Internal.initClientApp initJson app
        |> Effect.map \platformState -> {
            platformState: Box.box platformState,
            eventPreventDefault: Bool.false,
            eventStopPropagation: Bool.false,
        }
    else
        Html.Internal.dispatchEvent (Box.unbox eventPlatformState) eventJsonList eventHandlerId
        |> Effect.map \jsEventResult -> {
            platformState: Box.box jsEventResult.platformState,
            eventPreventDefault: jsEventResult.preventDefault,
            eventStopPropagation: jsEventResult.stopPropagation,
        }
