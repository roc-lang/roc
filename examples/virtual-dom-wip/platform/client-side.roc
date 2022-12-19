platform "client-side"
    requires {} { app : App state initData }
    exposes []
    packages {}
    imports [
        Html.Internal.{ App, JsEventResult, PlatformState },
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

main : FromHost state initData -> Effect (ToHost state initData)
main = \{ eventHandlerId, eventJsonList, eventPlatformState, initJson, isInitEvent } ->
    if hostInput.isInitEvent then
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
