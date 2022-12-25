platform "client-side"
    requires {} { app : App state initData }
    exposes []
    packages {}
    imports [
        Html.Internal.Shared.{ App, PlatformState },
        Html.Internal.Client.{ initClientApp, dispatchEvent },
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

# TODO: type checker bug! PlatformState doesn't match itself
main : FromHost state initData -> Effect (ToHost state initData) | initData has Decoding & Encoding
main = \fromHost ->
    if fromHost.isInitEvent then
        initClientApp fromHost.initJson app
        |> Effect.map \platformState -> {
            platformState: Box.box platformState,
            eventPreventDefault: Bool.false,
            eventStopPropagation: Bool.false,
        }
    else
        dispatchEvent (Box.unbox fromHost.eventPlatformState) fromHost.eventJsonList fromHost.eventHandlerId
        |> Effect.map \jsEventResult -> {
            platformState: Box.box jsEventResult.platformState,
            eventPreventDefault: jsEventResult.preventDefault,
            eventStopPropagation: jsEventResult.stopPropagation,
        }
